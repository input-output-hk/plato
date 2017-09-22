package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.validators._
import io.iohk.ethereum.ledger.BlockExecutionError.{StateBeforeFailure, TxsExecutionError, ValidationAfterExecError, ValidationBeforeExecError}
import io.iohk.ethereum.ledger.Ledger.{BlockPreparationResult, BlockResult, SimulateTxResult, TxResult}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators.{BlockValidator, SignedTransactionValidator}
import io.iohk.ethereum.domain.UInt256._
import io.iohk.ethereum.vm._
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec

trait Ledger {

  type S <: Storage[S]
  type W <: WorldStateProxy[W, S]

  def executeBlock(block: Block, validators: Validators): Either[BlockExecutionError, Seq[Receipt]]

  def prepareBlock(block: Block, validators: Validators): BlockPreparationResult[W, S]

  def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader): SimulateTxResult
}

//FIXME: Make Ledger independent of BlockchainImpl, for which it should become independent of WorldStateProxy type
class LedgerImpl(vm: VM, val blockchain: Blockchain, blockchainConfig: BlockchainConfig) extends Ledger with Logger {

  override type W = blockchain.WS
  override type S = blockchain.S

  type PC = ProgramContext[W, S]
  type PR = ProgramResult[W, S]

  val blockRewardCalculator = new BlockRewardCalculator(blockchainConfig.monetaryPolicyConfig)

  def executeBlock(
    block: Block,
    validators: Validators): Either[BlockExecutionError, Seq[Receipt]] = {

    val blockExecResult = for {
      _ <- validateBlockBeforeExecution(block, validators)

      execResult <- executeBlockTransactions(block, validators.signedTransactionValidator)
      BlockResult(resultingWorldStateProxy, gasUsed, receipts) = execResult
      worldToPersist = payBlockReward(block, resultingWorldStateProxy)
      worldPersisted = worldToPersist.persist() //State root hash needs to be up-to-date for validateBlockAfterExecution

      _ <- validateBlockAfterExecution(block, worldPersisted.stateRootHash, receipts, gasUsed, validators.blockValidator)
    } yield receipts

    if(blockExecResult.isRight)
      log.debug(s"Block ${block.header.number} (with hash: ${block.header.hashAsHexString}) executed correctly")
    blockExecResult
  }

  def prepareBlock(
    block: Block,
    validators: Validators): BlockPreparationResult[W, S] = {

    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = blockchain.getReadOnlyWorldStateProxy(None, blockchainConfig.accountStartNonce, parentStateRoot)
    val prepared = executePreparedTransactions(block.body.transactionList, initialWorld, block.header, validators.signedTransactionValidator)

    prepared match {
      case (execResult@BlockResult(resultingWorldStateProxy, _, _), txExecuted) =>
        val worldToPersist = payBlockReward(block, resultingWorldStateProxy)
        val worldPersisted = worldToPersist.persist()
        BlockPreparationResult(block.copy(body = block.body.copy(transactionList = txExecuted)), execResult, worldPersisted.stateRootHash)
    }
  }

  /**
    * This function runs transaction
    *
    * @param block
    * @param signedTransactionValidator
    */
  private[ledger] def executeBlockTransactions(
    block: Block,
    signedTransactionValidator: SignedTransactionValidator):
  Either[BlockExecutionError, BlockResult[W, S]] = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = blockchain.getWorldStateProxy(block.header.number, blockchainConfig.accountStartNonce, parentStateRoot)

    log.debug(s"About to execute ${block.body.transactionList.size} txs from block ${block.header.number} (with hash: ${block.header.hashAsHexString})")
    val blockTxsExecResult = executeTransactions(block.body.transactionList, initialWorld, block.header, signedTransactionValidator)
    blockTxsExecResult match {
      case Right(_) => log.debug(s"All txs from block ${block.header.hashAsHexString} were executed successfully")
      case Left(error) => log.debug(s"Not all txs from block ${block.header.hashAsHexString} were executed correctly, due to ${error.reason}")
    }
    blockTxsExecResult
  }

  @tailrec
  private[ledger] final def executePreparedTransactions(
    signedTransactions: Seq[SignedTransaction], world: WorldStateProxy[W, S],
    blockHeader: BlockHeader, signedTransactionValidator: SignedTransactionValidator,
    acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil, executed: Seq[SignedTransaction] = Nil): (BlockResult[W, S], Seq[SignedTransaction]) = {

    val result = executeTransactions(signedTransactions, world, blockHeader, signedTransactionValidator, acumGas, acumReceipts)

    result match {
      case Left(TxsExecutionError(stx, StateBeforeFailure(worldState, gas, receipts), reason)) =>
        log.debug(s"failure while preparing block because of $reason in transaction with hash ${stx.hashAsHexString}")
        val txIndex = signedTransactions.indexWhere(tx => tx.hash == stx.hash)
        executePreparedTransactions(signedTransactions.drop(txIndex + 1),
          worldState, blockHeader, signedTransactionValidator, gas, receipts, executed ++ signedTransactions.take(txIndex))
      case Right(br) => (br, executed ++ signedTransactions)
    }
  }

  /**
    * This functions executes all the signed transactions from a block (till one of those executions fails)
    *
    * @param signedTransactions from the block that are left to execute
    * @param world that will be updated by the execution of the signedTransactions
    * @param blockHeader of the block we are currently executing
    * @param signedTransactionValidator used to validate the signed transactions
    * @param acumGas, accumulated gas of the previoulsy executed transactions of the same block
    * @param acumReceipts, accumulated receipts of the previoulsy executed transactions of the same block
    * @return a BlockResult if the execution of all the transactions in the block was successful or a BlockExecutionError
    *         if one of them failed
    */
  @tailrec
  private[ledger] final def executeTransactions(signedTransactions: Seq[SignedTransaction], world: WorldStateProxy[W, S],
    blockHeader: BlockHeader, signedTransactionValidator: SignedTransactionValidator,
    acumGas: BigInt = 0, acumReceipts: Seq[Receipt] = Nil): Either[TxsExecutionError[W, S], BlockResult[W, S]] =
  signedTransactions match {
    case Nil =>
      Right(BlockResult(worldState = world, gasUsed = acumGas, receipts = acumReceipts))

    case Seq(stx, otherStxs@_*) =>
      val (senderAccount, worldForTx) = world.getAccount(stx.senderAddress).map(a => (a, world))
        .getOrElse(
          (Account.empty(blockchainConfig.accountStartNonce), world.saveAccount(stx.senderAddress, Account.empty(blockchainConfig.accountStartNonce)))
        )
      val upfrontCost = calculateUpfrontCost(stx.tx)
      val validatedStx = signedTransactionValidator.validate(stx, senderAccount, blockHeader, upfrontCost, acumGas)

      validatedStx match {
        case Right(_) =>
          val TxResult(newWorld, gasUsed, logs, _) = executeTransaction(stx, blockHeader, worldForTx)

          val receipt = Receipt(
            postTransactionStateHash = newWorld.stateRootHash,
            cumulativeGasUsed = acumGas + gasUsed,
            logsBloomFilter = BloomFilter.create(logs),
            logs = logs
          )

          log.debug(s"Receipt generated for tx ${stx.hashAsHexString}, $receipt")

          executeTransactions(otherStxs, newWorld, blockHeader, signedTransactionValidator, receipt.cumulativeGasUsed, acumReceipts :+ receipt)
        case Left(error) => Left(TxsExecutionError(stx, StateBeforeFailure(world, acumGas, acumReceipts), error.toString))
      }
  }

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader): SimulateTxResult = {
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
    val context: PC = prepareProgramContext[W, S](stx, blockHeader, worldForTx, config)

    val result = runVM(stx, context, config)

    val totalGasToRefund = calcTotalGasToRefund(stx, result)

    SimulateTxResult(gasLimit - totalGasToRefund, result.logs, result.returnData)
  }

  private[ledger] def executeTransaction[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (stx: SignedTransaction, blockHeader: BlockHeader, world: WorldStateProxy[W2, S2]): TxResult[W2, S2] = {

    log.debug(s"Transaction ${stx.hashAsHexString} execution start")
    val gasPrice = UInt256(stx.tx.gasPrice)
    val gasLimit = stx.tx.gasLimit
    val config = EvmConfig.forBlock(blockHeader.number, blockchainConfig)

    val checkpointWorldState = updateSenderAccountBeforeExecution[W2, S2](stx, world)
    val context = prepareProgramContext[W2, S2](stx, blockHeader, checkpointWorldState, config)
    val result = runVM(stx, context, config)

    val resultWithErrorHandling: ProgramResult[W2, S2] =
      if(result.error.isDefined) {
        //Rollback to the world before transfer was done if an error happened
        result.copy(world = checkpointWorldState, addressesToDelete = Set.empty, logs = Nil)
      } else
        result

    val totalGasToRefund = calcTotalGasToRefund(stx, resultWithErrorHandling)
    val executionGasToPayToMiner = gasLimit - totalGasToRefund

    val refundGasFn = pay[W2, S2](stx.senderAddress, (totalGasToRefund * gasPrice).toUInt256) _
    val payMinerForGasFn = pay[W2, S2](Address(blockHeader.beneficiary), (executionGasToPayToMiner * gasPrice).toUInt256) _
    val deleteAccountsFn: W2 => W2 = deleteAccounts[W2, S2](resultWithErrorHandling.addressesToDelete)
    val persistStateFn: W2 => W2 = world => world.persist()

    val world2 = (refundGasFn andThen payMinerForGasFn andThen deleteAccountsFn andThen persistStateFn)(resultWithErrorHandling.world)

    log.debug(
      s"""Transaction ${stx.hashAsHexString} execution end. Summary:
         | - Error: ${result.error}.
         | - Total Gas to Refund: $totalGasToRefund
         | - Execution gas paid to miner: $executionGasToPayToMiner""".stripMargin)

    TxResult(world2, gasLimit - totalGasToRefund, resultWithErrorHandling.logs, result.returnData)
  }

  private def validateBlockBeforeExecution(block: Block, validators: Validators): Either[BlockExecutionError, Unit] = {
    val result = for {
      _ <- validators.blockHeaderValidator.validate(block.header, blockchain)
      _ <- validators.blockValidator.validateHeaderAndBody(block.header, block.body)
      _ <- validators.ommersValidator.validate(block.header.number, block.body.uncleNodesList, blockchain)
    } yield ()
    result.left.map(error => ValidationBeforeExecError(error.toString))
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
    * @param blockValidator used to validate the receipts with the block
    * @return None if valid else a message with what went wrong
    */
  private[ledger] def validateBlockAfterExecution(block: Block, stateRootHash: ByteString, receipts: Seq[Receipt],
                                                  gasUsed: BigInt, blockValidator: BlockValidator): Either[BlockExecutionError, Unit] = {
    lazy val blockAndReceiptsValidation = blockValidator.validateBlockAndReceipts(block.header, receipts)
    if(block.header.gasUsed != gasUsed)
      Left(ValidationAfterExecError(s"Block has invalid gas used, expected ${block.header.gasUsed} but got $gasUsed"))
    else if(block.header.stateRoot != stateRootHash)
      Left(ValidationAfterExecError(
        s"Block has invalid state root hash, expected ${Hex.toHexString(block.header.stateRoot.toArray)} but got ${Hex.toHexString(stateRootHash.toArray)}")
      )
    else if(blockAndReceiptsValidation.isLeft)
      Left(ValidationAfterExecError(blockAndReceiptsValidation.left.get.toString))
    else
      Right(())
  }

  /**
    * This function updates state in order to pay rewards based on YP section 11.3
    *
    * @param block
    * @param worldStateProxy
    * @return
    */
  private[ledger] def payBlockReward[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (block: Block, worldStateProxy: WorldStateProxy[W2, S2]): W2 = {

    def getAccountToPay(address: Address, ws: WorldStateProxy[W2, S2]): Account = ws.getAccount(address)
      .getOrElse(Account.empty(blockchainConfig.accountStartNonce))

    val minerAddress = Address(block.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress, worldStateProxy)
    val minerReward = blockRewardCalculator.calcBlockMinerReward(block.header.number, block.body.uncleNodesList.size)
    val afterMinerReward = worldStateProxy.saveAccount(minerAddress, minerAccount.increaseBalance(UInt256(minerReward)))
    log.debug(s"Paying block ${block.header.number} reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress, ws)
      val ommerReward = blockRewardCalculator.calcOmmerMinerReward(block.header.number, ommer.number)
      log.debug(s"Paying block ${block.header.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(UInt256(ommerReward)))
    }
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
  private def updateSenderAccountBeforeExecution[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (stx: SignedTransaction, worldStateProxy: WorldStateProxy[W2, S2]): W2 = {
    val senderAddress = stx.senderAddress
    val account = worldStateProxy.getGuaranteedAccount(senderAddress)
    worldStateProxy.saveAccount(senderAddress, account.increaseBalance(-calculateUpfrontGas(stx.tx)).increaseNonce)
  }

  private def prepareProgramContext[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (stx: SignedTransaction, blockHeader: BlockHeader, worldStateProxy: W2,
                                            config: EvmConfig): ProgramContext[W2, S2] = {
    stx.tx.receivingAddress match {
      case None =>
        val address = worldStateProxy.createAddress(creatorAddr = stx.senderAddress)
        val worldAfterInitialisation = worldStateProxy.initialiseAccount(stx.senderAddress, address, UInt256(stx.tx.value))
        ProgramContext(stx, address,  Program(stx.tx.payload), blockHeader, worldAfterInitialisation, config)
      case Some(txReceivingAddress) =>
        val world1 = worldStateProxy.transfer(stx.senderAddress, txReceivingAddress, UInt256(stx.tx.value))
        ProgramContext(stx, txReceivingAddress, Program(world1.getCode(txReceivingAddress)), blockHeader, world1, config)
    }
  }

  private def runVM[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (stx: SignedTransaction, context: ProgramContext[W2, S2], config: EvmConfig): ProgramResult[W2, S2] = {
    val result: ProgramResult[W2, S2] = vm.run(context)
    if (stx.tx.isContractInit && result.error.isEmpty)
      saveNewContract(context.env.ownerAddr, result, config)
    else
      result
  }

  private def saveNewContract[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (address: Address, result: ProgramResult[W2, S2], config: EvmConfig): ProgramResult[W2, S2] = {
    val codeDepositCost = config.calcCodeDepositCost(result.returnData)
    if (result.gasRemaining < codeDepositCost) {
      if (config.exceptionalFailedCodeDeposit)
        result.copy(error = Some(OutOfGas))
      else
        result
    } else {
      result.copy(
        gasRemaining = result.gasRemaining - codeDepositCost,
        world = result.world.saveCode(address, result.returnData))
    }
  }

  /**
    * Calculate total gas to be refunded
    * See YP, eq (72)
    */
  private def calcTotalGasToRefund[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]](stx: SignedTransaction, result: ProgramResult[W2, S2]): BigInt = {
    if (result.error.isDefined)
      0
    else {
      val gasUsed = stx.tx.gasLimit - result.gasRemaining
      result.gasRemaining + (gasUsed / 2).min(result.gasRefund)
    }
  }

  private def pay[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]](address: Address, value: UInt256)(world: W2): W2 = {
    val account = world.getAccount(address).getOrElse(Account.empty(blockchainConfig.accountStartNonce)).increaseBalance(value)
    world.saveAccount(address, account)
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
  private[ledger] def deleteAccounts[W2 <: WorldStateProxy[W2, S2], S2 <: Storage[S2]]
    (addressesToDelete: Set[Address])(worldStateProxy: W2): W2 =
      addressesToDelete.foldLeft(worldStateProxy){ case (world, address) => world.deleteAccount(address) }

}

object Ledger {
  case class BlockResult[W <: WorldStateProxy[W, S], S <: Storage[S]](worldState: WorldStateProxy[W, S], gasUsed: BigInt = 0, receipts: Seq[Receipt] = Nil)
  case class BlockPreparationResult[W <: WorldStateProxy[W, S], S <: Storage[S]](block: Block, blockResult: BlockResult[W, S], stateRootHash: ByteString)
  case class TxResult[W <: WorldStateProxy[W, S], S <: Storage[S]]
    (worldState: WorldStateProxy[W, S], gasUsed: BigInt, logs: Seq[TxLogEntry], vmReturnData: ByteString)
  case class SimulateTxResult(gasUsed: BigInt, logs: Seq[TxLogEntry], vmReturnData: ByteString)
}

sealed trait BlockExecutionError{
  val reason: String
}

object BlockExecutionError {
  case class ValidationBeforeExecError(reason: String) extends BlockExecutionError
  case class StateBeforeFailure[W <: WorldStateProxy[W, S], S <: Storage[S]]
    (worldState: WorldStateProxy[W, S], acumGas: BigInt, acumReceipts: Seq[Receipt])
  case class TxsExecutionError[W <: WorldStateProxy[W, S], S <: Storage[S]]
    (stx: SignedTransaction, stateBeforeError: StateBeforeFailure[W, S], reason: String) extends BlockExecutionError
  case class ValidationAfterExecError(reason: String) extends BlockExecutionError
}


trait BlockPreparationError

case class TxError(reason: String) extends BlockPreparationError
