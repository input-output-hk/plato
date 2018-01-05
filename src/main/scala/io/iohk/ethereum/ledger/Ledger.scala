package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockExecutionError.{StateBeforeFailure, TxsExecutionError, ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.ledger.Ledger._
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, Logger}
import io.iohk.ethereum.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm._
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

trait Ledger {

  def checkBlockStatus(blockHash: ByteString): BlockStatus

  def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]]

  def prepareBlock(block: Block): BlockPreparationResult

  def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader): TxResult

  def importBlock(block: Block): BlockImportResult

  def resolveBranch(headers: Seq[SignedBlockHeader]): BranchResolutionResult

  def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader): BigInt
}

//FIXME: Make Ledger independent of BlockchainImpl, for which it should become independent of WorldStateProxy type
//TODO: EC-313: this has grown a bit large, consider splitting the aspects block import, block exec and TX exec
// scalastyle:off number.of.methods
// scalastyle:off file.size.limit
/**
  * Ledger handles importing and executing blocks.
  * Note: this class thread-unsafe because of its dependencies on Blockchain and BlockQueue
  */
class LedgerImpl(
    vm: VM,
    blockchain: BlockchainImpl,
    blockQueue: BlockQueue,
    blockchainConfig: BlockchainConfig,
    validators: Validators)
  extends Ledger with Logger {

  def this(vm: VM, blockchain: BlockchainImpl, blockchainConfig: BlockchainConfig,
    syncConfig: SyncConfig, validators: Validators) =
    this(vm, blockchain, BlockQueue(blockchain, syncConfig), blockchainConfig, validators)

  val blockRewardCalculator = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig)

  // scalastyle:off method.length
  /**
    * Tries to import the block as the new best block in the chain or enqueue it for later processing
    * @param block - block to be imported
    * @return One of:
    *         - [[BlockImportedToTop]] - if the block was added as the new best block
    *         - [[BlockEnqueued]] - block is stored in the [[BlockQueue]]
    *         - [[ChainReorganised]] - a better new branch was found causing chain reorganisation
    *         - [[DuplicateBlock]] - block already exists either in the main chain or in the queue
    *         - [[BlockImportFailed]] - block failed to execute (when importing to top or reorganising the chain)
    */
  def importBlock(block: Block): BlockImportResult = {
    if (block.signedHeader.header.number == 0 && blockchain.getSignedBlockHeaderByNumber(0).get.hash == block.signedHeader.hash) {
      log.debug(s"Ignoring duplicate genesis block: (${block.idTag})")
      DuplicateBlock
    } else {
      val validationResult = validateBlockBeforeExecution(block)
      validationResult match {
        case Left(ValidationBeforeExecError(HeaderParentNotFoundError)) =>
          log.debug(s"Block(${block.idTag}) has no known parent")
          UnknownParent

        case Left(ValidationBeforeExecError(reason)) =>
          log.debug(s"Block(${block.idTag}) failed pre-import validation")
          BlockImportFailed(reason.toString)

        case Right(_) =>
          val isDuplicate = blockchain.getBlockByHash(block.signedHeader.hash).isDefined || blockQueue.isQueued(block.signedHeader.hash)

          if (isDuplicate) {
            log.debug(s"Ignoring duplicate block: (${block.idTag})")
            DuplicateBlock
          }

          else {
            val bestBlock = blockchain.getBestBlock()
            val currentTd = blockchain.getTotalDifficultyByHash(bestBlock.signedHeader.hash).get

            val isTopOfChain = block.signedHeader.header.parentHash == bestBlock.signedHeader.hash

            if (isTopOfChain)
              importBlockToTop(block, bestBlock.signedHeader.header.number, currentTd)
            else
              enqueueBlockOrReorganiseChain(block, bestBlock, currentTd)
          }
      }
    }
  }

  private def importBlockToTop(block: Block, bestBlockNumber: BigInt, currentTd: BigInt): BlockImportResult = {
    val topBlockHash = blockQueue.enqueueBlock(block, bestBlockNumber).get.hash
    val topBlocks = blockQueue.getBranch(topBlockHash, dequeue = true)
    val (importedBlocks, maybeError) = executeBlocks(topBlocks, currentTd)
    val totalDifficulties = importedBlocks.foldLeft(List(currentTd)) {(tds, b) =>
      (tds.head + b.signedHeader.header.difficulty) :: tds
    }.reverse.tail

    val result = maybeError match {
      case None =>
        BlockImportedToTop(importedBlocks, totalDifficulties)

      case Some(error) if importedBlocks.isEmpty =>
        blockQueue.removeSubtree(block.signedHeader.hash)
        BlockImportFailed(error.toString)

      case Some(error) =>
        topBlocks.drop(importedBlocks.length).headOption.foreach { failedBlock =>
          blockQueue.removeSubtree(failedBlock.signedHeader.hash)
        }
        BlockImportedToTop(importedBlocks, totalDifficulties)
    }

    importedBlocks.foreach { b =>
      log.debug(s"Imported new block (${b.signedHeader.header.number}: ${Hex.toHexString(b.signedHeader.hash.toArray)}) to the top of chain")
    }
    result
  }


  private def enqueueBlockOrReorganiseChain(block: Block, bestBlock: Block, currentTd: BigInt): BlockImportResult = {
    // compares the total difficulties of branches, and resolves the tie by gas if enabled
    // yes, apparently only the gas from last block is checked:
    // https://github.com/ethereum/cpp-ethereum/blob/develop/libethereum/BlockChain.cpp#L811
    def isBetterBranch(newTd: BigInt) =
    newTd > currentTd ||
      (blockchainConfig.gasTieBreaker && newTd == currentTd && block.signedHeader.header.gasUsed > bestBlock.signedHeader.header.gasUsed)

    blockQueue.enqueueBlock(block, bestBlock.signedHeader.header.number) match {
      case Some(Leaf(leafHash, leafTd)) if isBetterBranch(leafTd) =>
        log.debug("Found a better chain, about to reorganise")
        reorganiseChainFromQueue(leafHash) match {
          case Right((oldBranch, newBranch)) =>
            val totalDifficulties = newBranch.tail.foldRight(List(leafTd)) { (b, tds) =>
              (tds.head - b.signedHeader.header.difficulty) :: tds
            }
            ChainReorganised(oldBranch, newBranch, totalDifficulties)

          case Left(error) =>
            BlockImportFailed(s"Error while trying to reorganise chain: $error")
        }

      case _ =>
        BlockEnqueued
    }
  }

  /**
    * Once a better branch was found this attempts to reorganise the chain
    * @param queuedLeaf - a block hash that determines a new branch stored in the queue (newest block from the branch)
    * @return [[BlockExecutionError]] if one of the blocks in the new branch failed to execute, otherwise:
    *        (oldBranch, newBranch) as lists of blocks
    */
  private def reorganiseChainFromQueue(queuedLeaf: ByteString): Either[BlockExecutionError, (List[Block], List[Block])] = {
    val newBranch = blockQueue.getBranch(queuedLeaf, dequeue = true)
    val parent = newBranch.head.signedHeader.header.parentHash
    val bestNumber = blockchain.getBestBlockNumber()
    val parentTd = blockchain.getTotalDifficultyByHash(parent).get

    val staleBlocksWithReceiptsAndTDs = removeBlocksUntil(parent, bestNumber).reverse
    val staleBlocks = staleBlocksWithReceiptsAndTDs.map(_._1)

    for (block <- staleBlocks) yield blockQueue.enqueueBlock(block)

    val (executedBlocks, maybeError) = executeBlocks(newBranch, parentTd)
    maybeError match {
      case None =>
        Right(staleBlocks, executedBlocks)

      case Some(error) =>
        revertChainReorganisation(newBranch, staleBlocksWithReceiptsAndTDs, executedBlocks)
        Left(error)
    }
  }

  /**
    * Used to revert chain reorganisation in the event that one of the blocks from new branch
    * fails to execute
    *
    * @param newBranch - new blocks
    * @param oldBranch - old blocks along with corresponding receipts and totalDifficulties
    * @param executedBlocks - sub-sequence of new branch that was executed correctly
    */
  private def revertChainReorganisation(newBranch: List[Block], oldBranch: List[(Block, Seq[Receipt], BigInt)],
    executedBlocks: List[Block]): Unit = {

    if (executedBlocks.nonEmpty) {
      removeBlocksUntil(executedBlocks.head.signedHeader.header.parentHash, executedBlocks.last.signedHeader.header.number)
    }

    oldBranch.foreach { case (block, receipts, td) =>
      blockchain.save(block, receipts, td, saveAsBestBlock = false)
    }

    val bestNumber = oldBranch.last._1.signedHeader.header.number
    blockchain.saveBestBlockNumber(bestNumber)
    executedBlocks.foreach(blockQueue.enqueueBlock(_, bestNumber))

    newBranch.diff(executedBlocks).headOption.foreach { block =>
      blockQueue.removeSubtree(block.signedHeader.hash)
    }
  }

  /**
    * Executes a list blocks, storing the results in the blockchain
    * @param blocks block to be executed
    * @return a list of blocks that were correctly executed and an optional [[BlockExecutionError]]
    */
  private def executeBlocks(blocks: List[Block], parentTd: BigInt): (List[Block], Option[BlockExecutionError]) = {
    blocks match {
      case block :: remainingBlocks =>
        executeBlock(block, alreadyValidated = true) match {
          case Right (receipts) =>
            val td = parentTd + block.signedHeader.header.difficulty
            blockchain.save(block, receipts, td, saveAsBestBlock = true)

            val (executedBlocks, error) = executeBlocks(remainingBlocks, td)
            (block :: executedBlocks, error)

          case Left(error) =>
          (Nil, Some(error))
        }

      case Nil =>
        (Nil, None)
    }
  }

  /**
    * Remove blocks from the [[Blockchain]] along with receipts and total difficulties
    * @param parent remove blocks until this hash (exclusive)
    * @param fromNumber start removing from this number (downwards)
    * @return the list of removed blocks along with receipts and total difficulties
    */
  private def removeBlocksUntil(parent: ByteString, fromNumber: BigInt): List[(Block, Seq[Receipt], BigInt)] = {
    blockchain.getBlockByNumber(fromNumber) match {
      case Some(block) if block.signedHeader.hash == parent =>
        Nil

      case Some(block) =>
        val receipts = blockchain.getReceiptsByHash(block.signedHeader.hash).get
        val td = blockchain.getTotalDifficultyByHash(block.signedHeader.hash).get

        //not updating best block number for efficiency, it will be updated in the callers anyway
        blockchain.removeBlock(block.signedHeader.hash, saveParentAsBestBlock = false)
        (block, receipts, td) :: removeBlocksUntil(parent, fromNumber - 1)

      case None =>
        log.error(s"Unexpected missing block number: $fromNumber")
        Nil
    }
  }

  /**
    * Finds a relation of a given list of headers to the current chain
    * Note:
    *   - the headers should form a chain (headers ordered by number)
    *   - last header number should be greater or equal than current best block number
    * @param headers - a list of headers to be checked
    * @return One of:
    *         - [[NewBetterBranch]] - the headers form a better branch than our current main chain
    *         - [[NoChainSwitch]] - the headers do not form a better branch
    *         - [[UnknownBranch]] - the parent of the first header is unknown (caller should obtain more headers)
    *         - [[InvalidBranch]] - headers do not form a chain or last header number is less than current best block number
    */
  def resolveBranch(headers: Seq[SignedBlockHeader]): BranchResolutionResult = {
    if (!doHeadersFormChain(headers) || headers.last.header.number < blockchain.getBestBlockNumber())
      InvalidBranch
    else {
      val parentIsKnown = blockchain.getSignedBlockHeaderByHash(headers.head.header.parentHash).isDefined

      // dealing with a situation when genesis block is included in the received headers, which may happen
      // in the early block of private networks
      val reachedGenesis = headers.head.header.number == 0 && blockchain.getSignedBlockHeaderByNumber(0).get.hash == headers.head.hash

      if (parentIsKnown || reachedGenesis) {
        // find blocks with same numbers in the current chain, removing any common prefix
        val (oldBranch, _) = getBlocksForHeaders(headers).zip(headers)
          .dropWhile{ case (oldBlock, newHeader) => oldBlock.signedHeader == newHeader }.unzip
        val newHeaders = headers.dropWhile(sh => oldBranch.headOption.exists(_.signedHeader.header.number > sh.header.number))

        val currentBranchDifficulty = oldBranch.map(_.signedHeader.header.difficulty).sum
        val newBranchDifficulty = newHeaders.map(_.header.difficulty).sum

        if (currentBranchDifficulty < newBranchDifficulty)
          NewBetterBranch(oldBranch)
        else
          NoChainSwitch
      }
      else
        UnknownBranch
    }
  }

  private def doHeadersFormChain(headers: Seq[SignedBlockHeader]): Boolean =
    if (headers.length > 1)
      headers.zip(headers.tail).forall {
        case (parent, child) =>
          parent.hash == child.header.parentHash && parent.header.number + 1 == child.header.number
      }
    else
      headers.nonEmpty

  private def getBlocksForHeaders(headers: Seq[SignedBlockHeader]): List[Block] = headers match {
    case Seq(signedBlockHeader, tail @ _*) =>
      blockchain.getBlockByNumber(signedBlockHeader.header.number).map(_ :: getBlocksForHeaders(tail)).getOrElse(Nil)
    case Seq() =>
      Nil
  }
  /**
    * Check current status of block, based on its hash
    *
    * @param blockHash - hash of block to check
    * @return One of:
    *         - [[InChain]] - Block already incorporated into blockchain
    *         - [[Queued]]  - Block in queue waiting to be resolved
    *         - [[UnknownBlock]] - Hash its not known to our client
    */
  def checkBlockStatus(blockHash: ByteString): BlockStatus = {
    if (blockchain.getBlockByHash(blockHash).isDefined)
      InChain
    else if (blockQueue.isQueued(blockHash))
      Queued
    else
      UnknownBlock
  }

  /**
    * Executes a block
    *
    * @param alreadyValidated should we skip pre-execution validation (if the block has already been validated,
    *                         eg. in the importBlock method)
    */
  def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]] = {

    val preExecValidationResult = if (alreadyValidated) Right(block) else validateBlockBeforeExecution(block)

    val blockExecResult = for {
      _ <- preExecValidationResult

      execResult <- executeBlockTransactions(block)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = payBlockReward(block, resultingWorldStateProxy)
      worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist) //State root hash needs to be up-to-date for validateBlockAfterExecution

      _ <- validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed)
    } yield receipts

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.signedHeader.header.number} (with hash: ${block.signedHeader.hashAsHexString}) executed correctly")
    blockExecResult
  }

  def prepareBlock(block: Block): BlockPreparationResult = {

    val parentStateRoot = blockchain.getSignedBlockHeaderByHash(block.signedHeader.header.parentHash).map(_.header.stateRoot)
    val initialWorld = blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, parentStateRoot)
    val prepared = executePreparedTransactions(block.body.transactionList, initialWorld, block.signedHeader.header)

    prepared match {
      case (execResult@BlockResult(resultingWorldStateProxy, _, _), txExecuted) =>
        val worldToPersist = payBlockReward(block, resultingWorldStateProxy)
        val worldPersisted = InMemoryWorldStateProxy.persistState(worldToPersist)
        BlockPreparationResult(block.copy(body = block.body.copy(transactionList = txExecuted)), execResult, worldPersisted.stateRootHash)
    }
  }

  /**
    * This function runs transaction
    *
    * @param block
    */
  private[ledger] def executeBlockTransactions(block: Block):
  Either[BlockExecutionError, BlockResult] = {
    val parentStateRoot = blockchain.getSignedBlockHeaderByHash(block.signedHeader.header.parentHash).map(_.header.stateRoot)
    val initialWorld =
      blockchain.getWorldStateProxy(
        block.signedHeader.header.number,
        blockchainConfig.accountStartNonce,
        parentStateRoot,
        EvmConfig.forBlock(block.signedHeader.header.number, blockchainConfig).noEmptyAccounts)

    val inputWorld = blockchainConfig.daoForkConfig match {
      case Some(daoForkConfig) if daoForkConfig.isDaoForkBlock(block.signedHeader.header.number) => drainDaoForkAccounts(initialWorld, daoForkConfig)
      case _ => initialWorld
    }

    log.debug(s"About to execute ${block.body.transactionList.size} txs from block " +
      s"${block.signedHeader.header.number} (with hash: ${block.signedHeader.hashAsHexString})")
    val blockTxsExecResult = executeTransactions(block.body.transactionList, inputWorld, block.signedHeader.header)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block ${block.signedHeader.hashAsHexString} were executed successfully")
      case Left(error) => log.debug(s"Not all txs from block ${block.signedHeader.hashAsHexString} were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  @tailrec
  private[ledger] final def executePreparedTransactions(
    signedTransactions: Seq[SignedTransaction], world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader, acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil,
    executed: Seq[SignedTransaction] = Nil): (BlockResult, Seq[SignedTransaction]) = {

    val result = executeTransactions(signedTransactions, world, blockHeader, acumGas, acumReceipts)

    result match {
      case Left(TxsExecutionError(stx, StateBeforeFailure(worldState, gas, receipts), reason)) =>
        log.debug(s"failure while preparing block because of $reason in transaction with hash ${stx.hashAsHexString}")
        val txIndex = signedTransactions.indexWhere(tx => tx.hash == stx.hash)
        executePreparedTransactions(signedTransactions.drop(txIndex + 1),
          worldState, blockHeader, gas, receipts, executed ++ signedTransactions.take(txIndex))
      case Right(br) => (br, executed ++ signedTransactions)
    }
  }

  /**
    * This functions executes all the signed transactions from a block (till one of those executions fails)
    *
    * @param signedTransactions from the block that are left to execute
    * @param world that will be updated by the execution of the signedTransactions
    * @param blockHeader of the block we are currently executing
    * @param acumGas, accumulated gas of the previoulsy executed transactions of the same block
    * @param acumReceipts, accumulated receipts of the previoulsy executed transactions of the same block
    * @return a BlockResult if the execution of all the transactions in the block was successful or a BlockExecutionError
    *         if one of them failed
    */
  @tailrec
  private[ledger] final def executeTransactions(signedTransactions: Seq[SignedTransaction], world: InMemoryWorldStateProxy,
    blockHeader: BlockHeader, acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil): Either[TxsExecutionError, BlockResult] =
  signedTransactions match {
    case Nil =>
      Right(BlockResult(worldState = world, gasUsed = acumGas, receipts = acumReceipts))

    case Seq(stx, otherStxs@_*) =>
      val (senderAccount, worldForTx) = world.getAccount(stx.senderAddress).map(a => (a, world))
        .getOrElse(
          (Account.empty(blockchainConfig.accountStartNonce), world.saveAccount(stx.senderAddress, Account.empty(blockchainConfig.accountStartNonce)))
        )
      val upfrontCost = calculateUpfrontCost(stx.tx)
      val validatedStx = validators.signedTransactionValidator.validate(stx, senderAccount, blockHeader, upfrontCost, acumGas)

      validatedStx match {
        case Right(_) =>
          val TxResult(newWorld, gasUsed, logs, _, _) = executeTransaction(stx, blockHeader, worldForTx)

          val receipt = Receipt(
            postTransactionStateHash = newWorld.stateRootHash,
            cumulativeGasUsed = acumGas + gasUsed,
            logsBloomFilter = BloomFilter.create(logs),
            logs = logs
          )

          log.debug(s"Receipt generated for tx ${stx.hashAsHexString}, $receipt")

          executeTransactions(otherStxs, newWorld, blockHeader, receipt.cumulativeGasUsed, acumReceipts :+ receipt)
        case Left(error) => Left(TxsExecutionError(stx, StateBeforeFailure(world, acumGas, acumReceipts), error.toString))
      }
  }

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader): TxResult = {
    val stateRoot = blockHeader.stateRoot

    val gasLimit = stx.tx.gasLimit
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val world1 = blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, Some(stateRoot))
    val world2 =
      if (world1.getAccount(stx.senderAddress).isEmpty)
        world1.saveAccount(stx.senderAddress, Account.empty(blockchainConfig.accountStartNonce))
      else
        world1

    val worldForTx = updateSenderAccountBeforeExecution(stx, world2)
    val context: PC = prepareProgramContext(stx, blockHeader, worldForTx, config)

    val result = runVM(stx, context, config)

    val totalGasToRefund = calcTotalGasToRefund(stx, result)

    TxResult(result.world, gasLimit - totalGasToRefund, result.logs, result.returnData, result.error)
  }

  def binarySearchGasEstimation(stx: SignedTransaction, blockHeader: BlockHeader): BigInt = {
    val lowLimit = EvmConfig.forBlock(blockHeader.number, blockchainConfig).feeSchedule.G_transaction
    val highLimit = stx.tx.gasLimit

    if (highLimit < lowLimit)
      highLimit
    else {
      LedgerUtils.binaryChop(lowLimit, highLimit)(gasLimit =>
        simulateTransaction(stx.copy(tx = stx.tx.copy(gasLimit = gasLimit)), blockHeader).vmError)
    }
  }

  private[ledger] def executeTransaction(stx: SignedTransaction, blockHeader: BlockHeader, world: InMemoryWorldStateProxy): TxResult = {
    log.debug(s"Transaction ${stx.hashAsHexString} execution start")
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = stx.tx.gasLimit
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val checkpointWorldState = updateSenderAccountBeforeExecution(stx, world)
    val context = prepareProgramContext(stx, blockHeader, checkpointWorldState, config)
    val result = runVM(stx, context, config)

    val resultWithErrorHandling: PR =
      if (result.error.isDefined) {
        //Rollback to the world before transfer was done if an error happened
        result.copy(world = checkpointWorldState, addressesToDelete = Set.empty, logs = Nil)
      } else
        result

    val totalGasToRefund = calcTotalGasToRefund(stx, resultWithErrorHandling)
    val executionGasToPayToMiner = gasLimit - totalGasToRefund

    val refundGasFn = pay(stx.senderAddress, (totalGasToRefund * gasPrice).toUInt256) _
    val payMinerForGasFn = pay(Address(blockHeader.beneficiary), (executionGasToPayToMiner * gasPrice).toUInt256) _

    val worldAfterPayments = (refundGasFn andThen payMinerForGasFn)(resultWithErrorHandling.world)

    val deleteAccountsFn = deleteAccounts(resultWithErrorHandling.addressesToDelete) _
    val deleteTouchedAccountsFn = deleteEmptyTouchedAccounts _
    val persistStateFn = InMemoryWorldStateProxy.persistState _

    val world2 = (deleteAccountsFn andThen deleteTouchedAccountsFn andThen persistStateFn)(worldAfterPayments)

    log.debug(
      s"""Transaction ${stx.hashAsHexString} execution end. Summary:
         | - Error: ${result.error}.
         | - Total Gas to Refund: $totalGasToRefund
         | - Execution gas paid to miner: $executionGasToPayToMiner""".stripMargin)

    TxResult(world2, executionGasToPayToMiner, resultWithErrorHandling.logs, result.returnData, result.error)
  }

  private def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] = {
    val result = for {
      _ <- validators.blockHeaderValidator.validate(block.signedHeader, getHeaderFromChainOrQueue _)
      _ <- validators.blockValidator.validateHeaderAndBody(block.signedHeader, block.body)
      // TODO: Remove it when ommer functionality dissapear
      /*_ <- validators.ommersValidator.validate(block.signedHeader.header.parentHash, block.signedHeader.header.number, block.body.uncleNodesList,
        getHeaderFromChainOrQueue, getNBlocksBackFromChainOrQueue)*/
    } yield BlockExecutionSuccess
    result.left.map(ValidationBeforeExecError)
  }

  /**
    * This function validates that the various results from execution are consistent with the block. This includes:
    *   - Validating the resulting stateRootHash
    *   - Doing BlockValidator.validateBlockReceipts validations involving the receipts
    *   - Validating the resulting gas used
    *
    * @param block to validate
    * @param stateRootHash from the resulting state trie after executing the txs from the block
    * @param receipts associated with the execution of each of the tx from the block
    * @param gasUsed, accumulated gas used for the execution of the txs from the block
    * @return None if valid else a message with what went wrong
    */
  private[ledger] def validateBlockAfterExecution(block: Block, stateRootHash: ByteString, receipts: Seq[Receipt],
                                                  gasUsed: BigInt): Either[BlockExecutionError, BlockExecutionSuccess] = {
    lazy val blockAndReceiptsValidation = validators.blockValidator.validateBlockAndReceipts(block.signedHeader, receipts)
    if(block.signedHeader.header.gasUsed != gasUsed)
      Left(ValidationAfterExecError(s"Block has invalid gas used, expected ${block.signedHeader.header.gasUsed} but got $gasUsed"))
    else if(block.signedHeader.header.stateRoot != stateRootHash)
      Left(ValidationAfterExecError(
        s"Block has invalid state root hash, expected ${Hex.toHexString(block.signedHeader.header.stateRoot.toArray)}" +
          s" but got ${Hex.toHexString(stateRootHash.toArray)}")
      )
    else if(blockAndReceiptsValidation.isLeft)
      Left(ValidationAfterExecError(blockAndReceiptsValidation.left.get.toString))
    else
      Right(BlockExecutionSuccess)
  }

  /**
    * This function updates state in order to pay rewards based on YP section 11.3
    *
    * @param block
    * @param worldStateProxy
    * @return
    */
  private[ledger] def payBlockReward(block: Block, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {

    def getAccountToPay(address: Address, ws: InMemoryWorldStateProxy): Account = ws.getAccount(address)
      .getOrElse(Account.empty(blockchainConfig.accountStartNonce))

    val minerAddress = Address(block.signedHeader.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress, worldStateProxy)
    val minerReward = blockRewardCalculator.calcBlockMinerReward(block.signedHeader.header.number, 0)
    log.debug(s"Paying block ${block.signedHeader.header.number} reward of $minerReward to miner with account address $minerAddress")
    worldStateProxy.saveAccount(minerAddress, minerAccount.increaseBalance(UInt256(minerReward)))
    // TODO: Remove it when ommer functionality dissapear
    /*block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)
      val ommerReward = blockRewardCalculator.calcOmmerMinerReward(block.signedHeader.header.number, ommer.number)
      log.debug(s"Paying block ${block.signedHeader.header.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(UInt256(ommerReward)))
    }*/
  }

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontGas(tx: Transaction): UInt256 = UInt256(tx.gasLimit * tx.gasPrice)

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontCost(tx: Transaction): UInt256 =
    UInt256(calculateUpfrontGas(tx) + tx.value)

  /**
    * Increments account nonce by 1 stated in YP equation (69) and
    * Pays the upfront Tx gas calculated as TxGasPrice * TxGasLimit from balance. YP equation (68)
    *
    * @param stx
    * @param worldStateProxy
    * @return
    */
  private def updateSenderAccountBeforeExecution(stx: SignedTransaction, worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    val senderAddress = stx.senderAddress
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.increaseBalance(-calculateUpfrontGas(stx.tx)).increaseNonce())
  }

  private def prepareProgramContext(stx: SignedTransaction, blockHeader: BlockHeader, world: InMemoryWorldStateProxy,
      config: EvmConfig): PC = {
    stx.tx.receivingAddress match {
      case None =>
        val address = world.createAddress(creatorAddr = stx.senderAddress)

        // EIP-684
        val conflict = world.nonEmptyCodeOrNonceAccount(address)
        val code = if (conflict) ByteString(INVALID.code) else stx.tx.payload

        val world1 = world.initialiseAccount(address).transfer(stx.senderAddress, address, UInt256(stx.tx.value))
        ProgramContext(stx, address,  Program(code), blockHeader, world1, config)

      case Some(txReceivingAddress) =>
        val world1 = world.transfer(stx.senderAddress, txReceivingAddress, UInt256(stx.tx.value))
        ProgramContext(stx, txReceivingAddress, Program(world1.getCode(txReceivingAddress)), blockHeader, world1, config)
    }
  }

  private def runVM(stx: SignedTransaction, context: PC, config: EvmConfig): PR = {
    val result: PR = vm.run(context)
    if (stx.tx.isContractInit && result.error.isEmpty)
      saveNewContract(context.env.ownerAddr, result, config)
    else
      result
  }

  private[ledger] def saveNewContract(address: Address, result: PR, config: EvmConfig): PR = {
    val contractCode = result.returnData
    val codeDepositCost = config.calcCodeDepositCost(contractCode)

    val maxCodeSizeExceeded = blockchainConfig.maxCodeSize.exists(codeSizeLimit => contractCode.size > codeSizeLimit)
    val codeStoreOutOfGas = result.gasRemaining < codeDepositCost

    if (maxCodeSizeExceeded || (codeStoreOutOfGas && config.exceptionalFailedCodeDeposit)) {
      // Code size too big or code storage causes out-of-gas with exceptionalFailedCodeDeposit enabled
      result.copy(error = Some(OutOfGas))
    } else if (codeStoreOutOfGas && !config.exceptionalFailedCodeDeposit) {
      // Code storage causes out-of-gas with exceptionalFailedCodeDeposit disabled
      result
    } else {
      // Code storage succeeded
      result.copy(
        gasRemaining = result.gasRemaining - codeDepositCost,
        world = result.world.saveCode(address, result.returnData))
    }
  }

  /**
    * Calculate total gas to be refunded
    * See YP, eq (72)
    */
  private def calcTotalGasToRefund(stx: SignedTransaction, result: PR): BigInt = {
    if (result.error.isDefined)
      0
    else {
      val gasUsed = stx.tx.gasLimit - result.gasRemaining
      result.gasRemaining + (gasUsed / 2).min(result.gasRefund)
    }
  }

  private[ledger] def pay(address: Address, value: UInt256)(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    if (world.isZeroValueTransferToNonExistentAccount(address, value)) {
      world
    } else {
      val account = world.getAccount(address).getOrElse(Account.empty(blockchainConfig.accountStartNonce)).increaseBalance(value)
      world.saveAccount(address, account).touchAccounts(address)
    }
  }

  /**
    * Delete all accounts (that appear in SUICIDE list). YP eq (78).
    * The contract storage should be cleared during pruning as nodes could be used in other tries.
    * The contract code is also not deleted as there can be contracts with the exact same code, making it risky to delete
    * the code of an account in case it is shared with another one.
    * FIXME: [EC-242]
    *   Should we delete the storage associated with the deleted accounts?
    *   Should we keep track of duplicated contracts for deletion?
    *
    * @param addressesToDelete
    * @param worldStateProxy
    * @return a worldState equal worldStateProxy except that the accounts from addressesToDelete are deleted
    */
  private[ledger] def deleteAccounts(addressesToDelete: Set[Address])(worldStateProxy: InMemoryWorldStateProxy): InMemoryWorldStateProxy =
    addressesToDelete.foldLeft(worldStateProxy){ case (world, address) => world.deleteAccount(address) }

  /**
    * This function updates worldState transferring balance from drainList accounts to refundContract address
    *
    * @param worldState Initial world state
    * @param daoForkConfig Dao fork configuration with drainList and refundContract config
    * @return Updated world state proxy
    */
  private def drainDaoForkAccounts(worldState: InMemoryWorldStateProxy, daoForkConfig: DaoForkConfig): InMemoryWorldStateProxy = {

    daoForkConfig.refundContract match {
      case Some(refundContractAddress) =>
        daoForkConfig.drainList.foldLeft(worldState) { (ws, address) =>
          ws.getAccount(address)
            .map(account => ws.transfer(from = address, to = refundContractAddress, account.balance))
            .getOrElse(ws)
        }
      case None => worldState
    }
  }

  /**
    * EIP161 - State trie clearing
    * Delete all accounts that have been touched (involved in any potentially state-changing operation) during transaction execution.
    *
    * All potentially state-changing operation are:
    * Account is the target or refund of a SUICIDE operation for zero or more value;
    * Account is the source or destination of a CALL operation or message-call transaction transferring zero or more value;
    * Account is the source or newly-creation of a CREATE operation or contract-creation transaction endowing zero or more value;
    * as the block author ("miner") it is recipient of block-rewards or transaction-fees of zero or more.
    *
    * Deletion of touched account should be executed immediately following the execution of the suicide list
    *
    * @param world world after execution of all potentially state-changing operations
    * @return a worldState equal worldStateProxy except that the accounts touched during execution are deleted and touched
    *         Set is cleared
    */
  private[ledger] def deleteEmptyTouchedAccounts(world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def deleteEmptyAccount(world: InMemoryWorldStateProxy, address: Address) = {
      if (world.getAccount(address).exists(_.isEmpty(blockchainConfig.accountStartNonce)))
        world.deleteAccount(address)
      else
        world
    }

    world.touchedAccounts
      .foldLeft(world)(deleteEmptyAccount)
      .clearTouchedAccounts
  }

  private def getHeaderFromChainOrQueue(hash: ByteString): Option[SignedBlockHeader] =
    blockchain.getSignedBlockHeaderByHash(hash).orElse(blockQueue.getBlockByHash(hash).map(_.signedHeader))

  private def getNBlocksBackFromChainOrQueue(hash: ByteString, n: Int): List[Block] = {
    val queuedBlocks = blockQueue.getBranch(hash, dequeue = false).take(n)
    if (queuedBlocks.length == n)
      queuedBlocks
    else {
      val chainedBlockHash = queuedBlocks.headOption.map(_.signedHeader.header.parentHash).getOrElse(hash)
      blockchain.getBlockByHash(chainedBlockHash) match {
        case None =>
          Nil

        case Some(block) =>
          val remaining = n - queuedBlocks.length - 1
          val numbers = (block.signedHeader.header.number - remaining) until block.signedHeader.header.number
          numbers.toList.flatMap(blockchain.getBlockByNumber) :+ block
      }
    }
  }
}

object Ledger {
  type PC = ProgramContext[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]
  type PR = ProgramResult[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]

  case class BlockResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: ByteString)
  case class TxResult(worldState: InMemoryWorldStateProxy, gasUsed: BigInt, logs: Seq[TxLogEntry],
    vmReturnData: ByteString, vmError: Option[ProgramError])
}

sealed trait BlockExecutionError{
  val reason: Any
}

sealed trait BlockExecutionSuccess
case object BlockExecutionSuccess extends BlockExecutionSuccess

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: Any) extends BlockExecutionError
  case class StateBeforeFailure(worldState: InMemoryWorldStateProxy, acumGas: BigInt, acumReceipts: Seq[Receipt])
  case class TxsExecutionError(stx: SignedTransaction, stateBeforeError: StateBeforeFailure, reason: String) extends BlockExecutionError
  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}

sealed trait BlockImportResult
case class BlockImportedToTop(imported: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case object BlockEnqueued extends BlockImportResult
case object DuplicateBlock extends BlockImportResult
case class ChainReorganised(oldBranch: List[Block], newBranch: List[Block], totalDifficulties: List[BigInt]) extends BlockImportResult
case class BlockImportFailed(error: String) extends BlockImportResult
case object UnknownParent extends BlockImportResult

sealed trait BranchResolutionResult
case class  NewBetterBranch(oldBranch: Seq[Block]) extends BranchResolutionResult
case object NoChainSwitch extends BranchResolutionResult
case object UnknownBranch extends BranchResolutionResult
case object InvalidBranch extends BranchResolutionResult

sealed trait BlockStatus
case object InChain       extends BlockStatus
case object Queued        extends BlockStatus
case object UnknownBlock  extends BlockStatus

trait BlockPreparationError

case class TxError(reason: String) extends BlockPreparationError
case object LockedMinerAccountError extends BlockPreparationError
