package io.iohk.ethereum.jsonrpc

import akka.pattern.ask
import akka.util.Timeout
import io.iohk.ethereum.domain.{BlockHeader, SignedTransaction, UInt256, _}
import akka.actor.ActorRef
import io.iohk.ethereum.db.storage.AppStateStorage
import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.jsonrpc.FilterManager.{FilterChanges, FilterLogs, LogFilterLogs, TxLog}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.mining.BlockGenerator
import io.iohk.ethereum.utils._
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.UInt256RLPImplicits._
import org.spongycastle.util.encoders.Hex

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

// scalastyle:off number.of.methods number.of.types
object EthService {

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class BestBlockNumberRequest()
  case class BestBlockNumberResponse(bestBlockNumber: BigInt)

  case class TxCountByBlockHashRequest(blockHash: ByteString)
  case class TxCountByBlockHashResponse(txsQuantity: Option[Int])

  case class BlockByBlockHashRequest(blockHash: ByteString, fullTxs: Boolean)
  case class BlockByBlockHashResponse(blockResponse: Option[BlockResponse])

  case class BlockByNumberRequest(block: BlockParam, fullTxs: Boolean)
  case class BlockByNumberResponse(blockResponse: Option[BlockResponse])

  case class GetTransactionByBlockHashAndIndexRequest(blockHash: ByteString, transactionIndex: BigInt)
  case class GetTransactionByBlockHashAndIndexResponse(transactionResponse: Option[TransactionResponse])

  case class UncleByBlockHashAndIndexRequest(blockHash: ByteString, uncleIndex: BigInt)
  case class UncleByBlockHashAndIndexResponse(uncleBlockResponse: Option[BlockResponse])

  case class UncleByBlockNumberAndIndexRequest(block: BlockParam, uncleIndex: BigInt)
  case class UncleByBlockNumberAndIndexResponse(uncleBlockResponse: Option[BlockResponse])

  case class GetTransactionByHashRequest(txHash: ByteString)
  case class GetTransactionByHashResponse(txResponse: Option[TransactionResponse])

  case class GetAccountTransactionsRequest(address: Address, fromBlock: BigInt, toBlock: BigInt)
  case class GetAccountTransactionsResponse(sent: Seq[TransactionResponse], received: Seq[TransactionResponse])

  case class GetTransactionReceiptRequest(txHash: ByteString)
  case class GetTransactionReceiptResponse(txResponse: Option[TransactionReceiptResponse])

  case class GetTransactionByBlockNumberAndIndexRequest(block: BlockParam, transactionIndex: BigInt)
  case class GetTransactionByBlockNumberAndIndexResponse(transactionResponse: Option[TransactionResponse])

  case class GetGasPriceRequest()
  case class GetGasPriceResponse(price: BigInt)

  case class SyncingRequest()
  case class SyncingStatus(startingBlock: BigInt, currentBlock: BigInt, highestBlock: BigInt)
  case class SyncingResponse(syncStatus: Option[SyncingStatus])

  case class GetHashRateRequest()
  case class GetHashRateResponse(hashRate: BigInt)

  case class GetMiningRequest()
  case class GetMiningResponse(isMining: Boolean)

  case class SendRawTransactionRequest(data: ByteString)
  case class SendRawTransactionResponse(transactionHash: ByteString)

  sealed trait BlockParam

  object BlockParam {
    case class WithNumber(n: BigInt) extends BlockParam
    case object Latest extends BlockParam
    case object Pending extends BlockParam
    case object Earliest extends BlockParam
  }

  case class CallTx(
    from: Option[ByteString],
    to: Option[ByteString],
    gas: Option[BigInt],
    gasPrice: BigInt,
    value: BigInt,
    data: ByteString)
  case class CallRequest(tx: CallTx, block: BlockParam)
  case class CallResponse(returnData: ByteString)
  case class EstimateGasResponse(gas: BigInt)

  case class GetCodeRequest(address: Address, block: BlockParam)
  case class GetCodeResponse(result: ByteString)

  case class GetUncleCountByBlockNumberRequest(block: BlockParam)
  case class GetUncleCountByBlockNumberResponse(result: BigInt)

  case class GetUncleCountByBlockHashRequest(blockHash: ByteString)
  case class GetUncleCountByBlockHashResponse(result: BigInt)

  case class GetBlockTransactionCountByNumberRequest(block: BlockParam)
  case class GetBlockTransactionCountByNumberResponse(result: BigInt)

  case class GetBalanceRequest(address: Address, block: BlockParam)
  case class GetBalanceResponse(value: BigInt)

  case class GetStorageAtRequest(address: Address, position: BigInt, block: BlockParam)
  case class GetStorageAtResponse(value: ByteString)

  case class GetTransactionCountRequest(address: Address, block: BlockParam)
  case class GetTransactionCountResponse(value: BigInt)

  case class ResolvedBlock(block: Block, pending: Boolean)

  case class NewFilterRequest(filter: Filter)
  case class Filter(
      fromBlock: Option[BlockParam],
      toBlock: Option[BlockParam],
      address: Option[Address],
      topics: Seq[Seq[ByteString]])

  case class NewBlockFilterRequest()
  case class NewPendingTransactionFilterRequest()

  case class NewFilterResponse(filterId: BigInt)

  case class UninstallFilterRequest(filterId: BigInt)
  case class UninstallFilterResponse(success: Boolean)

  case class GetFilterChangesRequest(filterId: BigInt)
  case class GetFilterChangesResponse(filterChanges: FilterChanges)

  case class GetFilterLogsRequest(filterId: BigInt)
  case class GetFilterLogsResponse(filterLogs: FilterLogs)

  case class GetLogsRequest(filter: Filter)
  case class GetLogsResponse(filterLogs: LogFilterLogs)
}

class EthService(
    blockchain: Blockchain,
    blockGenerator: BlockGenerator,
    appStateStorage: AppStateStorage,
    miningConfig: MiningConfig,
    ledger: Ledger,
    keyStore: KeyStore,
    pendingTransactionsManager: ActorRef,
    syncingController: ActorRef,
    ommersPool: ActorRef,
    filterManager: ActorRef,
    filterConfig: FilterConfig,
    blockchainConfig: BlockchainConfig,
    protocolVersion: Int)
  extends Logger {

  import EthService._

  def protocolVersion(req: ProtocolVersionRequest): ServiceResponse[ProtocolVersionResponse] =
    Future.successful(Right(ProtocolVersionResponse(f"0x$protocolVersion%x")))

  /**
    * eth_blockNumber that returns the number of most recent block.
    *
    * @return Current block number the client is on.
    */
  def bestBlockNumber(req: BestBlockNumberRequest): ServiceResponse[BestBlockNumberResponse] = Future {
    Right(BestBlockNumberResponse(appStateStorage.getBestBlockNumber()))
  }

  /**
    * Implements the eth_getBlockTransactionCountByHash method that fetches the number of txs that a certain block has.
    *
    * @param request with the hash of the block requested
    * @return the number of txs that the block has or None if the client doesn't have the block requested
    */
  def getBlockTransactionCountByHash(request: TxCountByBlockHashRequest): ServiceResponse[TxCountByBlockHashResponse] = Future {
    val txsCount = blockchain.getBlockBodyByHash(request.blockHash).map(_.transactionList.size)
    Right(TxCountByBlockHashResponse(txsCount))
  }

  /**
    * Implements the eth_getBlockByHash method that fetches a requested block.
    *
    * @param request with the hash of the block requested
    * @return the block requested or None if the client doesn't have the block
    */
  def getByBlockHash(request: BlockByBlockHashRequest): ServiceResponse[BlockByBlockHashResponse] = Future {
    val BlockByBlockHashRequest(blockHash, fullTxs) = request
    val blockOpt = blockchain.getBlockByHash(blockHash)
    val totalDifficulty = blockchain.getTotalDifficultyByHash(blockHash)

    val blockResponseOpt = blockOpt.map(block => BlockResponse(block, totalDifficulty, fullTxs = fullTxs))
    Right(BlockByBlockHashResponse(blockResponseOpt))
  }

  /**
    * Implements the eth_getBlockByNumber method that fetches a requested block.
    *
    * @param request with the block requested (by it's number or by tag)
    * @return the block requested or None if the client doesn't have the block
    */
  def getBlockByNumber(request: BlockByNumberRequest): ServiceResponse[BlockByNumberResponse] = Future {
    val BlockByNumberRequest(blockParam, fullTxs) = request
    val blockResponseOpt = resolveBlock(blockParam).toOption.map { case ResolvedBlock(block, pending) =>
      val totalDifficulty = blockchain.getTotalDifficultyByHash(block.signedHeader.hash)
      BlockResponse(block, totalDifficulty, fullTxs = fullTxs, pendingBlock = pending)
    }
    Right(BlockByNumberResponse(blockResponseOpt))
  }

  /**
    * Implements the eth_getTransactionByHash method that fetches a requested tx.
    * The tx requested will be fetched from the pending tx pool or from the already executed txs (depending on the tx state)
    *
    * @param req with the tx requested (by it's hash)
    * @return the tx requested or None if the client doesn't have the tx
    */
  def getTransactionByHash(req: GetTransactionByHashRequest): ServiceResponse[GetTransactionByHashResponse] = {
    val maybeTxPendingResponse: Future[Option[TransactionResponse]] = getTransactionsFromPool.map{
      _.pendingTransactions.map(_.stx).find(_.hash == req.txHash).map(TransactionResponse(_)) }

    val maybeTxResponse: Future[Option[TransactionResponse]] = maybeTxPendingResponse.flatMap{ txPending =>
      Future { txPending.orElse{
        for {
          TransactionLocation(blockHash, txIndex) <- blockchain.getTransactionLocation(req.txHash)
          Block(signedHeader, body) <- blockchain.getBlockByHash(blockHash)
          stx <- body.transactionList.lift(txIndex)
        } yield TransactionResponse(stx, Some(signedHeader), Some(txIndex))
      }}
    }

    maybeTxResponse.map(txResponse => Right(GetTransactionByHashResponse(txResponse)))
  }

  def getTransactionReceipt(req: GetTransactionReceiptRequest): ServiceResponse[GetTransactionReceiptResponse] = Future {
    val result: Option[TransactionReceiptResponse] = for {
      TransactionLocation(blockHash, txIndex) <- blockchain.getTransactionLocation(req.txHash)
      Block(signedHeader, body) <- blockchain.getBlockByHash(blockHash)
      stx <- body.transactionList.lift(txIndex)
      receipts <- blockchain.getReceiptsByHash(blockHash)
      receipt: Receipt <- receipts.lift(txIndex)
    } yield {

      val contractAddress = if (stx.tx.isContractInit) {
        //do not subtract 1 from nonce because in transaction we have nonce of account before transaction execution
        val hash = kec256(rlp.encode(RLPList(stx.senderAddress.bytes, UInt256(stx.tx.nonce).toRLPEncodable)))
        Some(Address(hash))
      } else {
        None
      }

      TransactionReceiptResponse(
        transactionHash = stx.hash,
        transactionIndex = txIndex,
        blockNumber = signedHeader.header.number,
        blockHash = signedHeader.hash,
        cumulativeGasUsed = receipt.cumulativeGasUsed,
        gasUsed = if (txIndex == 0) receipt.cumulativeGasUsed else receipt.cumulativeGasUsed - receipts(txIndex - 1).cumulativeGasUsed,
        contractAddress = contractAddress,
        logs = receipt.logs.zipWithIndex.map { case (txLog, index) =>
          TxLog(
            logIndex = index,
            transactionIndex = txIndex,
            transactionHash = stx.hash,
            blockHash = signedHeader.hash,
            blockNumber = signedHeader.header.number,
            address = txLog.loggerAddress,
            data = txLog.data,
            topics = txLog.logTopics)
        })
    }

    Right(GetTransactionReceiptResponse(result))
  }

  /**
    * eth_getTransactionByBlockHashAndIndex that returns information about a transaction by block hash and
    * transaction index position.
    *
    * @return the tx requested or None if the client doesn't have the block or if there's no tx in the that index
    */
  def getTransactionByBlockHashAndIndexRequest(req: GetTransactionByBlockHashAndIndexRequest)
  : ServiceResponse[GetTransactionByBlockHashAndIndexResponse] = Future {
    import req._
    val maybeTransactionResponse = blockchain.getBlockByHash(blockHash).flatMap{
      blockWithTx =>
        val blockTxs = blockWithTx.body.transactionList
        if (transactionIndex >= 0 && transactionIndex < blockTxs.size)
          Some(TransactionResponse(blockTxs(transactionIndex.toInt), Some(blockWithTx.signedHeader), Some(transactionIndex.toInt)))
        else None
    }
    Right(GetTransactionByBlockHashAndIndexResponse(maybeTransactionResponse))
  }

  /**
    * Implements the eth_getUncleByBlockHashAndIndex method that fetches an uncle from a certain index in a requested block.
    *
    * @param request with the hash of the block and the index of the uncle requested
    * @return the uncle that the block has at the given index or None if the client doesn't have the block or if there's no uncle in that index
    */
  def getUncleByBlockHashAndIndex(request: UncleByBlockHashAndIndexRequest): ServiceResponse[UncleByBlockHashAndIndexResponse] = Future {
    val UncleByBlockHashAndIndexRequest(blockHash, uncleIndex) = request
    val uncleHeaderOpt = blockchain.getBlockBodyByHash(blockHash)
      .flatMap { body =>
        if (uncleIndex >= 0 && uncleIndex < body.uncleNodesList.size)
          Some(body.uncleNodesList.apply(uncleIndex.toInt))
        else
          None
      }
    val totalDifficulty = uncleHeaderOpt.flatMap(uncleHeader => blockchain.getTotalDifficultyByHash(uncleHeader.hash))

    //The block in the response will not have any txs or uncles
    val uncleBlockResponseOpt = uncleHeaderOpt.map { uncleHeader =>
      BlockResponse(signedBlockHeader = uncleHeader, totalDifficulty = totalDifficulty, pendingBlock = false) }
    Right(UncleByBlockHashAndIndexResponse(uncleBlockResponseOpt))
  }

  /**
    * Implements the eth_getUncleByBlockNumberAndIndex method that fetches an uncle from a certain index in a requested block.
    *
    * @param request with the number/tag of the block and the index of the uncle requested
    * @return the uncle that the block has at the given index or None if the client doesn't have the block or if there's no uncle in that index
    */
  def getUncleByBlockNumberAndIndex(request: UncleByBlockNumberAndIndexRequest): ServiceResponse[UncleByBlockNumberAndIndexResponse] = Future {
    val UncleByBlockNumberAndIndexRequest(blockParam, uncleIndex) = request
    val uncleBlockResponseOpt = resolveBlock(blockParam).toOption
      .flatMap { case ResolvedBlock(block, pending) =>
        if (uncleIndex >= 0 && uncleIndex < block.body.uncleNodesList.size) {
          val uncleHeader = block.body.uncleNodesList.apply(uncleIndex.toInt)
          val totalDifficulty = blockchain.getTotalDifficultyByHash(uncleHeader.hash)

          //The block in the response will not have any txs or uncles
          Some(BlockResponse(signedBlockHeader = uncleHeader, totalDifficulty = totalDifficulty, pendingBlock = pending))
        } else
          None
      }

    Right(UncleByBlockNumberAndIndexResponse(uncleBlockResponseOpt))
  }

  def getGetGasPrice(req: GetGasPriceRequest): ServiceResponse[GetGasPriceResponse] = {
    val blockDifference = 30
    val bestBlock = appStateStorage.getBestBlockNumber()

    Future{
      val gasPrice = ((bestBlock - blockDifference) to bestBlock)
        .flatMap(blockchain.getBlockByNumber)
        .flatMap(_.body.transactionList)
        .map(_.tx.gasPrice)
      if (gasPrice.nonEmpty) {
        val avgGasPrice = gasPrice.sum / gasPrice.length
        Right(GetGasPriceResponse(avgGasPrice))
      } else {
        Right(GetGasPriceResponse(0))
      }
    }
  }

  private def getOmmersFromPool(blockNumber: BigInt) = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (ommersPool ? OmmersPool.GetOmmers(blockNumber)).mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error("failed to get ommer, mining block with empty ommers list", ex)
        OmmersPool.Ommers(Nil)
      }
  }

  private def getTransactionsFromPool = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error("failed to get transactions, mining block with empty transactions list", ex)
        PendingTransactionsResponse(Nil)
      }
  }

  /**
    * Implements the eth_syncing method that returns syncing information if the node is syncing.
    *
    * @return The syncing status if the node is syncing or None if not
    */
 def syncing(req: SyncingRequest): ServiceResponse[SyncingResponse] = Future {
   val currentBlock = appStateStorage.getBestBlockNumber()
   val highestBlock = appStateStorage.getEstimatedHighestBlock()

   //The node is syncing if there's any block that other peers have and this peer doesn't
   val maybeSyncStatus =
     if(currentBlock < highestBlock)
       Some(SyncingStatus(
         startingBlock = appStateStorage.getSyncStartingBlock(),
         currentBlock = currentBlock,
         highestBlock = highestBlock
       ))
     else
       None
   Right(SyncingResponse(maybeSyncStatus))
 }
  /**
    * The client doesn't use PoW anymore, so fetching for hash rate has no longer sense.
    * We fixed the value to 0 in order to maintain compatibility with tools like etc-netstats,
    * that use this method from the JSON RPC interface.
    */
  def getHashRate(req: GetHashRateRequest): ServiceResponse[GetHashRateResponse] = {
    Future.successful(Right(GetHashRateResponse(0)))
  }

  /**
    * The client doesn't use PoW anymore, so fetching for 'is mining' has no longer sense.
    * We fixed the value to true in order to maintain compatibility with tools like etc-netstats,
    * that use this method from the JSON RPC interface.
    */
  def getMining(req: GetMiningRequest): ServiceResponse[GetMiningResponse] = {
    Future.successful(Right(GetMiningResponse(true)))
  }

  def sendRawTransaction(req: SendRawTransactionRequest): ServiceResponse[SendRawTransactionResponse] = {
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionDec

    Try(req.data.toArray.toSignedTransaction) match {
      case Success(signedTransaction) =>
        pendingTransactionsManager ! PendingTransactionsManager.AddOrOverrideTransaction(signedTransaction)
        Future.successful(Right(SendRawTransactionResponse(signedTransaction.hash)))
      case Failure(_) =>
        Future.successful(Left(JsonRpcErrors.InvalidRequest))
    }
  }

  def call(req: CallRequest): ServiceResponse[CallResponse] = {
    Future {
      doCall(req)(ledger.simulateTransaction).map(r => CallResponse(r.vmReturnData))
    }
  }

  def estimateGas(req: CallRequest): ServiceResponse[EstimateGasResponse] = {
    Future {
      doCall(req)(ledger.binarySearchGasEstimation).map(gasUsed => EstimateGasResponse(gasUsed))
    }
  }

  def getCode(req: GetCodeRequest): ServiceResponse[GetCodeResponse] = {
    Future {
      resolveBlock(req.block).map { case ResolvedBlock(block, _) =>
        val world = blockchain.getWorldStateProxy(
          block.signedHeader.header.number,
          blockchainConfig.accountStartNonce,
          Some(block.signedHeader.header.stateRoot)
        )
        GetCodeResponse(world.getCode(req.address))
      }
    }
  }

  def getUncleCountByBlockNumber(req: GetUncleCountByBlockNumberRequest): ServiceResponse[GetUncleCountByBlockNumberResponse] = {
    Future {
      resolveBlock(req.block).map { case ResolvedBlock(block, _) =>
        GetUncleCountByBlockNumberResponse(block.body.uncleNodesList.size)
      }
    }
  }

  def getUncleCountByBlockHash(req: GetUncleCountByBlockHashRequest): ServiceResponse[GetUncleCountByBlockHashResponse] = {
    Future {
      blockchain.getBlockBodyByHash(req.blockHash) match {
        case Some(blockBody) =>
          Right(GetUncleCountByBlockHashResponse(blockBody.uncleNodesList.size))
        case None =>
          Left(JsonRpcErrors.InvalidParams(s"Block with hash ${Hex.toHexString(req.blockHash.toArray[Byte])} not found"))
      }
    }
  }

  def getBlockTransactionCountByNumber(req: GetBlockTransactionCountByNumberRequest): ServiceResponse[GetBlockTransactionCountByNumberResponse] = {
    Future {
      resolveBlock(req.block).map { case ResolvedBlock(block, _) =>
        GetBlockTransactionCountByNumberResponse(block.body.transactionList.size)
      }
    }
  }

  def getTransactionByBlockNumberAndIndexRequest(req: GetTransactionByBlockNumberAndIndexRequest):
  ServiceResponse[GetTransactionByBlockNumberAndIndexResponse] = Future {
    import req._
    resolveBlock(block).map{
      blockWithTx =>
        val blockTxs = blockWithTx.block.body.transactionList
        if (transactionIndex >= 0 && transactionIndex < blockTxs.size)
          GetTransactionByBlockNumberAndIndexResponse(
            Some(TransactionResponse(blockTxs(transactionIndex.toInt),
              Some(blockWithTx.block.signedHeader),
              Some(transactionIndex.toInt))))
        else
          GetTransactionByBlockNumberAndIndexResponse(None)
    }.left.flatMap(_ => Right(GetTransactionByBlockNumberAndIndexResponse(None)))
  }

  def getBalance(req: GetBalanceRequest): ServiceResponse[GetBalanceResponse] = {
    Future {
      withAccount(req.address, req.block) { account =>
        GetBalanceResponse(account.balance)
      }
    }
  }

  def getStorageAt(req: GetStorageAtRequest): ServiceResponse[GetStorageAtResponse] = {
    Future {
      withAccount(req.address, req.block) { account =>
        GetStorageAtResponse(blockchain.getAccountStorageAt(account.storageRoot, req.position))
      }
    }
  }

  def getTransactionCount(req: GetTransactionCountRequest): ServiceResponse[GetTransactionCountResponse] = {
    Future {
      withAccount(req.address, req.block) { account =>
        GetTransactionCountResponse(account.nonce)
      }
    }
  }

  def newFilter(req: NewFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    import req.filter._
    (filterManager ? FilterManager.NewLogFilter(fromBlock, toBlock, address, topics)).mapTo[FilterManager.NewFilterResponse].map { resp =>
      Right(NewFilterResponse(resp.id))
    }
  }

  def newBlockFilter(req: NewBlockFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    (filterManager ? FilterManager.NewBlockFilter).mapTo[FilterManager.NewFilterResponse].map { resp =>
      Right(NewFilterResponse(resp.id))
    }
  }

  def newPendingTransactionFilter(req: NewPendingTransactionFilterRequest): ServiceResponse[NewFilterResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    (filterManager ? FilterManager.NewPendingTransactionFilter).mapTo[FilterManager.NewFilterResponse].map { resp =>
      Right(NewFilterResponse(resp.id))
    }
  }

  def uninstallFilter(req: UninstallFilterRequest): ServiceResponse[UninstallFilterResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    (filterManager ? FilterManager.UninstallFilter(req.filterId)).map { _ =>
      Right(UninstallFilterResponse(success = true))
    }
  }

  def getFilterChanges(req: GetFilterChangesRequest): ServiceResponse[GetFilterChangesResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    (filterManager ? FilterManager.GetFilterChanges(req.filterId)).mapTo[FilterManager.FilterChanges].map { filterChanges =>
      Right(GetFilterChangesResponse(filterChanges))
    }
  }

  def getFilterLogs(req: GetFilterLogsRequest): ServiceResponse[GetFilterLogsResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)

    (filterManager ? FilterManager.GetFilterLogs(req.filterId)).mapTo[FilterManager.FilterLogs].map { filterLogs =>
      Right(GetFilterLogsResponse(filterLogs))
    }
  }

  def getLogs(req: GetLogsRequest): ServiceResponse[GetLogsResponse] = {
    implicit val timeout = Timeout(filterConfig.filterManagerQueryTimeout)
    import req.filter._

    (filterManager ? FilterManager.GetLogs(fromBlock, toBlock, address, topics)).mapTo[FilterManager.LogFilterLogs].map { filterLogs =>
      Right(GetLogsResponse(filterLogs))
    }
  }

  private def withAccount[T](address: Address, blockParam: BlockParam)(f: Account => T): Either[JsonRpcError, T] = {
    resolveBlock(blockParam).map { case ResolvedBlock(block, _) =>
      f(blockchain.getAccount(address, block.signedHeader.header.number).getOrElse(Account.empty(blockchainConfig.accountStartNonce)))
    }
  }

  private def resolveBlock(blockParam: BlockParam): Either[JsonRpcError, ResolvedBlock] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
      blockchain.getBlockByNumber(number)
        .map(Right.apply)
        .getOrElse(Left(JsonRpcErrors.InvalidParams(s"Block $number not found")))
    }

    blockParam match {
      case BlockParam.WithNumber(blockNumber) => getBlock(blockNumber).map(ResolvedBlock(_, pending = false))
      case BlockParam.Earliest => getBlock(0).map(ResolvedBlock(_, pending = false))
      case BlockParam.Latest => getBlock(appStateStorage.getBestBlockNumber()).map(ResolvedBlock(_, pending = false))
      case BlockParam.Pending =>
        blockGenerator.getPending.map(pb => ResolvedBlock(pb.block, pending = true))
          .map(Right.apply)
          .getOrElse(resolveBlock(BlockParam.Latest)) //Default behavior in other clients
    }
  }

  private def doCall[A](req: CallRequest)(f: (SignedTransaction, BlockHeader) => A): Either[JsonRpcError, A] = for {
    stx   <- prepareTransaction(req)
    block <- resolveBlock(req.block)
  } yield f(stx, block.block.signedHeader.header)

  private def getGasLimit(req: CallRequest): Either[JsonRpcError, BigInt] =
    if (req.tx.gas.isDefined) Right[JsonRpcError, BigInt](req.tx.gas.get)
    else resolveBlock(BlockParam.Latest).map(r => r.block.signedHeader.header.gasLimit)

  private def prepareTransaction(req: CallRequest):Either[JsonRpcError ,SignedTransaction] = {
    getGasLimit(req).map{ gasLimit =>
      val fromAddress = req.tx.from
        .map(Address.apply) // `from` param, if specified
        .getOrElse(
        keyStore
          .listAccounts().getOrElse(Nil).headOption // first account, if exists and `from` param not specified
          .getOrElse(Address(0))) // 0x0 default

      val toAddress = req.tx.to.map(Address.apply)

      val tx = Transaction(0, req.tx.gasPrice, gasLimit, toAddress, req.tx.value, req.tx.data)
      val fakeSignature = ECDSASignature(0, 0, 0.toByte)
      SignedTransaction(tx, fakeSignature, fromAddress)
    }
  }

  def getAccountTransactions(request: GetAccountTransactionsRequest): ServiceResponse[GetAccountTransactionsResponse] = {
    import Config.Network.Rpc.accountTransactionsMaxBlocks
    val numBlocksToSearch = request.toBlock - request.fromBlock
    if (numBlocksToSearch > accountTransactionsMaxBlocks) {
      Future.successful(Left(JsonRpcErrors.InvalidParams(
        s"""Maximum number of blocks to search is $accountTransactionsMaxBlocks, requested: $numBlocksToSearch.
           |See: 'network.rpc.account-transactions-max-blocks' config.""".stripMargin)))
    } else {

      def collectTxs(blockHeader: Option[BlockHeader], pending: Boolean): PartialFunction[SignedTransaction, TransactionResponse] = {
        case stx if stx.senderAddress == request.address =>
          TransactionResponse(stx, blockHeader, pending = Some(pending), isOutgoing = Some(true))
        case stx if stx.tx.receivingAddress.contains(request.address) =>
          TransactionResponse(stx, blockHeader, pending = Some(pending), isOutgoing = Some(false))
      }

      getTransactionsFromPool map { case PendingTransactionsResponse(pendingTransactions) =>
        val pendingTxs = pendingTransactions
          .map(_.stx)
          .collect(collectTxs(None, pending = true))

        val txsFromBlocks = (request.toBlock to request.fromBlock by -1)
          .toStream
          .flatMap { n => blockchain.getBlockByNumber(n) }
          .flatMap { block =>
            block.body.transactionList.collect(collectTxs(Some(block.signedHeader.header), pending = false)).reverse
          }

        Right(GetAccountTransactionsResponse(pendingTxs ++ txsFromBlocks))
      }
    }
  }
}
