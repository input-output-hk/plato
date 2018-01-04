package io.iohk.ethereum.jsonrpc

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.Fixtures.FakeSignature
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.{Fixtures, NormalPatience, Timeouts, crypto}
import io.iohk.ethereum.domain.{Address, Block, BlockHeader, BlockchainImpl, UInt256, _}
import io.iohk.ethereum.db.storage.{AppStateStorage, ArchiveNodeStorage}
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.Await
import io.iohk.ethereum.jsonrpc.EthService.ProtocolVersionRequest
import io.iohk.ethereum.jsonrpc.FilterManager.TxLog
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.mining.{BlockGenerator, PendingBlock}
import io.iohk.ethereum.mpt.{ByteArrayEncoder, ByteArraySerializable, HashByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.transactions.PendingTransactionsManager.{PendingTransaction, PendingTransactionsResponse}
import io.iohk.ethereum.validators.Validators
import org.scalamock.scalatest.MockFactory
import org.spongycastle.util.encoders.Hex

// scalastyle:off file.size.limit
class EthServiceSpec extends FlatSpec with Matchers with ScalaFutures with MockFactory with NormalPatience {

  behavior of "EthService"

  it should "answer eth_blockNumber with the latest block number" in new TestSetup {
    val bestBlockNumber = 10
    (appStateStorage.getBestBlockNumber _).expects().returning(bestBlockNumber)

    val response = Await.result(ethService.bestBlockNumber(BestBlockNumberRequest()), Duration.Inf).right.get
    response.bestBlockNumber shouldEqual bestBlockNumber
  }

  it should "return ethereum protocol version" in new TestSetup {
    val response = ethService.protocolVersion(ProtocolVersionRequest())
    val protocolVersion = response.futureValue.right.get.value

    Integer.parseInt(protocolVersion.drop(2), 16) shouldEqual currentProtocolVersion
  }

  it should "answer eth_getBlockTransactionCountByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe None
  }

  it should "answer eth_getBlockTransactionCountByHash with the block has no tx when the requested block is in the blockchain and has no tx" in new TestSetup {
    blockchain.save(blockToRequest.copy(body = BlockBody(Nil, Nil)))
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe Some(0)
  }

  it should "answer eth_getBlockTransactionCountByHash correctly when the requested block is in the blockchain and has some tx" in new TestSetup {
    blockchain.save(blockToRequest)
    val request = TxCountByBlockHashRequest(blockToRequestHash)
    val response = Await.result(ethService.getBlockTransactionCountByHash(request), Duration.Inf).right.get
    response.txsQuantity shouldBe Some(blockToRequest.body.transactionList.size)
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no block with the requested hash" in new TestSetup {
    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.signedHeader.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf).right.get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with None when there is no tx in requested index" in new TestSetup {
    blockchain.save(blockToRequest)

    val invalidTxIndex = blockToRequest.body.transactionList.size
    val requestWithInvalidIndex = GetTransactionByBlockHashAndIndexRequest(blockToRequest.signedHeader.hash, invalidTxIndex)
    val response = Await.result(
      ethService.getTransactionByBlockHashAndIndexRequest(requestWithInvalidIndex),
      Duration.Inf
    ).right.get

    response.transactionResponse shouldBe None
  }

  it should "answer eth_getTransactionByBlockHashAndIndex with the transaction response correctly when the requested index has one" in new TestSetup {
    blockchain.save(blockToRequest)

    val txIndexToRequest = blockToRequest.body.transactionList.size / 2
    val request = GetTransactionByBlockHashAndIndexRequest(blockToRequest.signedHeader.hash, txIndexToRequest)
    val response = Await.result(ethService.getTransactionByBlockHashAndIndexRequest(request), Duration.Inf).right.get

    val requestedStx = blockToRequest.body.transactionList.apply(txIndexToRequest)
    val expectedTxResponse = TransactionResponse(requestedStx, Some(blockToRequest.signedHeader), Some(txIndexToRequest))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  it should "answer eth_getBlockByNumber with the correct block when the pending block is requested" in new TestSetup {

    (blockGenerator.getPending _).expects().returns(Some(PendingBlock(blockToRequest, Nil)))

    val request = BlockByNumberRequest(BlockParam.Pending, fullTxs = true)
    val response = ethService.getBlockByNumber(request).futureValue.right.get

    response.blockResponse.isDefined should be (true)
    val blockResponse = response.blockResponse.get

    blockResponse.hash shouldBe None
    blockResponse.nonce shouldBe None
    blockResponse.miner shouldBe None
    blockResponse.number shouldBe blockToRequest.signedHeader.header.number
  }

  it should "answer eth_getBlockByNumber with the latest block pending block is requested and there are no pending ones" in new TestSetup {

    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    (blockGenerator.getPending _).expects().returns(None)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.signedHeader.header.number)

    val request = BlockByNumberRequest(BlockParam.Pending, fullTxs = true)
    val response = ethService.getBlockByNumber(request).futureValue.right.get
    response.blockResponse.get.hash.get shouldEqual blockToRequest.signedHeader.hash
  }

  it should "answer eth_getBlockByNumber with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.signedHeader), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.signedHeader), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.totalDifficulty shouldBe None
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByNumber with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByNumberRequest(BlockParam.WithNumber(blockToRequestNumber), fullTxs = true)
    val response = Await.result(ethService.getBlockByNumber(request.copy(fullTxs = false)), Duration.Inf).right.get

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getBlockByHash with None when the requested block isn't in the blockchain" in new TestSetup {
    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get
    response.blockResponse shouldBe None
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's totalDifficulty is in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.signedHeader), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when it's totalDifficulty is not in blockchain" in new TestSetup {
    blockchain.save(blockToRequest)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request), Duration.Inf).right.get

    val stxResponses = blockToRequest.body.transactionList.zipWithIndex.map { case (stx, txIndex) =>
      TransactionResponse(stx, Some(blockToRequest.signedHeader), Some(txIndex))
    }

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = true))
    response.blockResponse.get.totalDifficulty shouldBe None
    response.blockResponse.get.transactions.right.toOption shouldBe Some(stxResponses)
  }

  it should "answer eth_getBlockByHash with the block response correctly when the txs should be hashed" in new TestSetup {
    blockchain.save(blockToRequest)
    blockchain.save(blockToRequestHash, blockTd)

    val request = BlockByBlockHashRequest(blockToRequestHash, fullTxs = true)
    val response = Await.result(ethService.getByBlockHash(request.copy(fullTxs = false)), Duration.Inf).right.get

    response.blockResponse shouldBe Some(BlockResponse(blockToRequest, fullTxs = false, totalDifficulty = Some(blockTd)))
    response.blockResponse.get.totalDifficulty shouldBe Some(blockTd)
    response.blockResponse.get.transactions.left.toOption shouldBe Some(blockToRequest.body.transactionList.map(_.hash))
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle" in new TestSetup {
    blockchain.save(blockToRequest)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response1 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = 1)), Duration.Inf).right.get
    val response2 = Await.result(ethService.getUncleByBlockHashAndIndex(request.copy(uncleIndex = -1)), Duration.Inf).right.get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockHashAndIndex correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockHashAndIndex correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)
    blockchain.save(uncle.hash, uncleTd)

    val uncleIndexToRequest = 0
    val request = UncleByBlockHashAndIndexRequest(blockToRequestHash, uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockHashAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd), pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when the requested block isn't in the blockchain" in new TestSetup {
    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get
    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle" in new TestSetup {
    blockchain.save(blockToRequest)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex with None when there's no uncle in the requested index" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response1 = Await.result(ethService.getUncleByBlockNumberAndIndex(request.copy(uncleIndex = 1)), Duration.Inf).right.get
    val response2 = Await.result(ethService.getUncleByBlockNumberAndIndex(request.copy(uncleIndex = -1)), Duration.Inf).right.get

    response1.uncleBlockResponse shouldBe None
    response2.uncleBlockResponse shouldBe None
  }

  it should "answer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one but there's no total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, None, pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe None
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "anwer eth_getUncleByBlockNumberAndIndex correctly when the requested index has one and there's total difficulty for it" in new TestSetup {
    blockchain.save(blockToRequestWithUncles)
    blockchain.save(uncle.hash, uncleTd)

    val uncleIndexToRequest = 0
    val request = UncleByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequestNumber), uncleIndexToRequest)
    val response = Await.result(ethService.getUncleByBlockNumberAndIndex(request), Duration.Inf).right.get

    response.uncleBlockResponse shouldBe Some(BlockResponse(uncle, Some(uncleTd), pendingBlock = false))
    response.uncleBlockResponse.get.totalDifficulty shouldBe Some(uncleTd)
    response.uncleBlockResponse.get.transactions shouldBe Left(Nil)
    response.uncleBlockResponse.get.uncles shouldBe Nil
  }

  it should "return syncing info if the peer is syncing" in new TestSetup {
    (appStateStorage.getSyncStartingBlock _).expects().returning(999)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(10000)
    (appStateStorage.getBestBlockNumber _).expects().returning(200)
    val response = ethService.syncing(SyncingRequest()).futureValue.right.get

    response shouldEqual SyncingResponse(Some(EthService.SyncingStatus(
      startingBlock = 999,
      currentBlock = 200,
      highestBlock = 10000
    )))
  }

  it should "return no syncing info if the peer is not syncing" in new TestSetup {
    (appStateStorage.getSyncStartingBlock _).expects().returning(999)
    (appStateStorage.getEstimatedHighestBlock _).expects().returning(1000)
    (appStateStorage.getBestBlockNumber _).expects().returning(1000)
    val response = ethService.syncing(SyncingRequest()).futureValue.right.get

    response shouldEqual SyncingResponse(None)
  }

  it should "return requested work" in new TestSetup {
    (blockGenerator.generateBlockForMining _).expects(parentBlock, Nil, *, *).returning(Right(PendingBlock(block, Nil)))
    blockchain.save(parentBlock, Nil, parentBlock.header.difficulty, true)

    val response: ServiceResponse[GetWorkResponse] = ethService.getWork(GetWorkRequest())
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsManager.PendingTransactionsResponse(Nil))

    ommersPool.expectMsg(OmmersPool.GetOmmers(1))
    ommersPool.reply(OmmersPool.Ommers(Nil))

    response.futureValue shouldEqual Right(GetWorkResponse(powHash, seedHash, target))
  }

  it should "accept submitted correct PoW" in new TestSetup {
    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(Some(PendingBlock(block, Nil)))
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethService.submitWork(req)
    response.futureValue shouldEqual Right(SubmitWorkResponse(true))
  }

  it should "reject submitted correct PoW when header is no longer in cache" in new TestSetup {
    val headerHash = ByteString(Hex.decode("01" * 32))

    (blockGenerator.getPrepared _).expects(headerHash).returning(None)
    (appStateStorage.getBestBlockNumber _).expects().returning(0)

    val req = SubmitWorkRequest(ByteString("nonce"), headerHash, ByteString(Hex.decode("01" * 32)))

    val response = ethService.submitWork(req)
    response.futureValue shouldEqual Right(SubmitWorkResponse(false))
  }

  it should "execute call and return a value" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.signedHeader.header.number)

    val txResult = TxResult(BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None), 123, Nil, ByteString("return_value"), None)
    (ledger.simulateTransaction _).expects(*, *).returning(txResult)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1), 2, 3, ByteString(""))
    val response = ethService.call(CallRequest(tx, BlockParam.Latest))

    response.futureValue shouldEqual Right(CallResponse(ByteString("return_value")))
  }

  it should "execute estimateGas and return a value" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.signedHeader.header.number)

    val estimatedGas = BigInt(123)
    (ledger.binarySearchGasEstimation _).expects(*, *).returning(estimatedGas)

    val tx = CallTx(
      Some(ByteString(Hex.decode("da714fe079751fa7a1ad80b76571ea6ec52a446c"))),
      Some(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477"))),
      Some(1), 2, 3, ByteString(""))
    val response = ethService.estimateGas(CallRequest(tx, BlockParam.Latest))

    response.futureValue shouldEqual Right(EstimateGasResponse(123))
  }

  it should "get uncle count by block number" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.signedHeader.header.number)

    val response = ethService.getUncleCountByBlockNumber(GetUncleCountByBlockNumberRequest(BlockParam.Latest))

    response.futureValue shouldEqual Right(GetUncleCountByBlockNumberResponse(blockToRequest.body.uncleNodesList.size))
  }

  it should "get uncle count by block hash" in new TestSetup {
    blockchain.save(blockToRequest)

    val response = ethService.getUncleCountByBlockHash(GetUncleCountByBlockHashRequest(blockToRequest.signedHeader.hash))

    response.futureValue shouldEqual Right(GetUncleCountByBlockHashResponse(blockToRequest.body.uncleNodesList.size))
  }

  it should "get transaction count by block number" in new TestSetup {
    blockchain.save(blockToRequest)

    val response = ethService.getBlockTransactionCountByNumber(GetBlockTransactionCountByNumberRequest(BlockParam.WithNumber(blockToRequest.signedHeader.header.number)))

    response.futureValue shouldEqual Right(GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size))
  }

  it should "get transaction count by latest block number" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returning(blockToRequest.signedHeader.header.number)

    val response = ethService.getBlockTransactionCountByNumber(GetBlockTransactionCountByNumberRequest(BlockParam.Latest))

    response.futureValue shouldEqual Right(GetBlockTransactionCountByNumberResponse(blockToRequest.body.transactionList.size))
  }

  it should "handle getCode request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))
    storagesInstance.storages.evmCodeStorage.put(ByteString("code hash"), ByteString("code code code"))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](new ArchiveNodeStorage(storagesInstance.storages.nodeStorage))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(0), ByteString(""), ByteString("code hash")))

    val newBlockHeader = blockToRequest.signedHeader.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newSignedBlockHeader = blockToRequest.signedHeader.copy(header = newBlockHeader)
    val newblock = blockToRequest.copy(signedHeader = newSignedBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.signedHeader.header.number)

    val response = ethService.getCode(GetCodeRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetCodeResponse(ByteString("code code code")))
  }

  it should "return 0 gas price if there are no transactions" in new TestSetup {
    (appStateStorage.getBestBlockNumber _).expects().returning(42)

    val response = ethService.getGetGasPrice(GetGasPriceRequest())
    response.futureValue shouldEqual Right(GetGasPriceResponse(0))
  }

  it should "return average gas price" in new TestSetup {
    (appStateStorage.getBestBlockNumber _).expects().returning(42)
    val newBlockHeader = Fixtures.Blocks.Block3125369.signedHeader.header.copy(number = 42)
    val newSignedBlockHeader = Fixtures.Blocks.Block3125369.signedHeader.copy(header = newBlockHeader)
    blockchain.save(Block(newSignedBlockHeader, Fixtures.Blocks.Block3125369.body))

    val response = ethService.getGetGasPrice(GetGasPriceRequest())
    response.futureValue shouldEqual Right(GetGasPriceResponse(BigInt("20000000000")))
  }

  it should "getTransactionByBlockNumberAndIndexRequest return transaction by index" in new TestSetup {
    blockchain.save(blockToRequest)
    (appStateStorage.getBestBlockNumber _).expects().returns(blockToRequest.signedHeader.header.number)

    val txIndex: Int = 1
    val request = GetTransactionByBlockNumberAndIndexRequest(BlockParam.Latest, txIndex)
    val response = Await.result(ethService.getTransactionByBlockNumberAndIndexRequest(request), Duration.Inf).right.get

    val expectedTxResponse = TransactionResponse(blockToRequest.body.transactionList(txIndex), Some(blockToRequest.signedHeader), Some(txIndex))
    response.transactionResponse shouldBe Some(expectedTxResponse)
  }

  it should "getTransactionByBlockNumberAndIndexRequest return empty response if transaction does not exists when getting by index" in new TestSetup {
    blockchain.save(blockToRequest)

    val txIndex: Int = blockToRequest.body.transactionList.length + 42
    val request = GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.signedHeader.header.number), txIndex)
    val response = Await.result(ethService.getTransactionByBlockNumberAndIndexRequest(request), Duration.Inf).right.get

    response.transactionResponse shouldBe None
  }

  it should "getTransactionByBlockNumberAndIndexRequest return empty response if block does not exists when getting by index" in new TestSetup {
    blockchain.save(blockToRequest)

    val txIndex: Int = 1
    val request = GetTransactionByBlockNumberAndIndexRequest(BlockParam.WithNumber(blockToRequest.signedHeader.header.number - 42), txIndex)
    val response = Await.result(ethService.getTransactionByBlockNumberAndIndexRequest(request), Duration.Inf).right.get

    response.transactionResponse shouldBe None
  }

  it should "handle getBalance request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](new ArchiveNodeStorage(storagesInstance.storages.nodeStorage))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(123), ByteString(""), ByteString("code hash")))

    val newBlockHeader = blockToRequest.signedHeader.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newSignedBlockHeader = blockToRequest.signedHeader.copy(header =  newBlockHeader)
    val newblock = blockToRequest.copy(signedHeader = newSignedBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.signedHeader.header.number)

    val response = ethService.getBalance(GetBalanceRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetBalanceResponse(123))
  }

  it should "handle getStorageAt request" in new TestSetup {
    import io.iohk.ethereum.rlp.UInt256RLPImplicits._

    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val byteArrayUInt256Serializer = new ByteArrayEncoder[UInt256] {
      override def toBytes(input: UInt256): Array[Byte] = input.bytes.toArray[Byte]
    }

    val rlpUInt256Serializer = new ByteArraySerializable[UInt256] {
      override def fromBytes(bytes: Array[Byte]): UInt256 = ByteString(bytes).toUInt256
      override def toBytes(input: UInt256): Array[Byte] = input.toBytes
    }

    val storageMpt =
      MerklePatriciaTrie[UInt256, UInt256](new ArchiveNodeStorage(storagesInstance.storages.nodeStorage))(
        HashByteArraySerializable(byteArrayUInt256Serializer), rlpUInt256Serializer)
        .put(UInt256(333), UInt256(123))

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](new ArchiveNodeStorage(storagesInstance.storages.nodeStorage))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(0, UInt256(0), ByteString(storageMpt.getRootHash), ByteString("")))

    val newBlockHeader = blockToRequest.signedHeader.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newSignedBlockHeader = blockToRequest.signedHeader.copy(header =  newBlockHeader)
    val newblock = blockToRequest.copy(signedHeader = newSignedBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.signedHeader.header.number)

    val response = ethService.getStorageAt(GetStorageAtRequest(address, 333, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetStorageAtResponse(UInt256(123).bytes))
  }

  it should "handle get transaction count request" in new TestSetup {
    val address = Address(ByteString(Hex.decode("abbb6bebfa05aa13e908eaa492bd7a8343760477")))

    import MerklePatriciaTrie.defaultByteArraySerializable

    val mpt =
      MerklePatriciaTrie[Array[Byte], Account](new ArchiveNodeStorage(storagesInstance.storages.nodeStorage))
        .put(crypto.kec256(address.bytes.toArray[Byte]), Account(999, UInt256(0), ByteString(""), ByteString("")))

    val newBlockHeader = blockToRequest.signedHeader.header.copy(stateRoot = ByteString(mpt.getRootHash))
    val newSignedBlockHeader = blockToRequest.signedHeader.copy(header =  newBlockHeader)
    val newblock = blockToRequest.copy(signedHeader = newSignedBlockHeader)
    blockchain.save(newblock)
    (appStateStorage.getBestBlockNumber _).expects().returning(newblock.signedHeader.header.number)

    val response = ethService.getTransactionCount(GetTransactionCountRequest(address, BlockParam.Latest))

    response.futureValue shouldEqual Right(GetTransactionCountResponse(BigInt(999)))
  }

  it should "handle get transaction by hash if the tx is not on the blockchain and not in the tx pool" in new TestSetup {
    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethService.getTransactionByHash(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response.futureValue shouldEqual Right(GetTransactionByHashResponse(None))
  }

  it should "handle get transaction by hash if the tx is still pending" in new TestSetup {
    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethService.getTransactionByHash(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Seq(PendingTransaction(txToRequest, System.currentTimeMillis))))

    response.futureValue shouldEqual Right(GetTransactionByHashResponse(Some(TransactionResponse(txToRequest))))
  }

  it should "handle get transaction by hash if the tx was already executed" in new TestSetup {
    val blockWithTx = Block(Fixtures.Blocks.Block3125369.signedHeader, Fixtures.Blocks.Block3125369.body)
    blockchain.save(blockWithTx)

    val request = GetTransactionByHashRequest(txToRequestHash)
    val response = ethService.getTransactionByHash(request)

    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))

    response.futureValue shouldEqual Right(GetTransactionByHashResponse(Some(
      TransactionResponse(txToRequest, Some(blockWithTx.signedHeader), Some(0)))))
  }

  it should "calculate correct contract address for contract creating by transaction" in new TestSetup {
    val body = BlockBody(Seq(Fixtures.Blocks.Block3125369.body.transactionList.head, contractCreatingTransaction), Nil)
    val blockWithTx = Block(Fixtures.Blocks.Block3125369.signedHeader, body)
    blockchain.save(blockWithTx)
    val gasUsedByTx = 4242
    blockchain.save(Fixtures.Blocks.Block3125369.signedHeader.hash,
      Seq(fakeReceipt, fakeReceipt.copy(cumulativeGasUsed = fakeReceipt.cumulativeGasUsed + gasUsedByTx)))

    val request = GetTransactionReceiptRequest(contractCreatingTransaction.hash)
    val response = ethService.getTransactionReceipt(request)

    response.futureValue shouldEqual Right(GetTransactionReceiptResponse(Some(
      TransactionReceiptResponse(
        transactionHash = contractCreatingTransaction.hash,
        transactionIndex = 1,
        blockNumber = Fixtures.Blocks.Block3125369.signedHeader.header.number,
        blockHash = Fixtures.Blocks.Block3125369.signedHeader.hash,
        cumulativeGasUsed = fakeReceipt.cumulativeGasUsed + gasUsedByTx,
        gasUsed = gasUsedByTx,
        contractAddress = Some(createdContractAddress),
        logs = Seq(TxLog(
          logIndex = 0,
          transactionIndex = 1,
          transactionHash = contractCreatingTransaction.hash,
          blockHash = Fixtures.Blocks.Block3125369.signedHeader.hash,
          blockNumber = Fixtures.Blocks.Block3125369.signedHeader.header.number,
          address = fakeReceipt.logs.head.loggerAddress,
          data = fakeReceipt.logs.head.data,
          topics = fakeReceipt.logs.head.logTopics
        ))))))
  }

  it should "return account recent transactions in newest -> oldest order" in new TestSetup {
    val address = Address("0xee4439beb5c71513b080bbf9393441697a29f478")

    val keyPair = crypto.generateKeyPair(new SecureRandom)

    val tx1 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 1, ByteString()), keyPair, None)
    val tx2 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 2, ByteString()), keyPair, None)
    val tx3 = SignedTransaction.sign(Transaction(0, 123, 456, Some(address), 3, ByteString()), keyPair, None)

    val blockWithTx1 = Block(Fixtures.Blocks.Block3125369.header, Fixtures.Blocks.Block3125369.body.copy(
      transactionList = Seq(tx1)))

    val blockWithTxs2and3 = Block(Fixtures.Blocks.Block3125369.header.copy(number = 3125370), Fixtures.Blocks.Block3125369.body.copy(
      transactionList = Seq(tx2, tx3)))

    blockchain.save(blockWithTx1)
    blockchain.save(blockWithTxs2and3)

    val request = GetAccountTransactionsRequest(address, 3125360, 3125370)

    val response = ethService.getAccountTransactions(request)
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Nil))


    val expectedReceived = Seq(TransactionResponse(
      Fixtures.Blocks.Block3125369.body.transactionList.head,
      blockHeader = Some(Fixtures.Blocks.Block3125369.signedHeader),
      pending = Some(false)))
    response.futureValue shouldEqual Right(GetAccountTransactionsResponse(Nil, expectedReceived))
  }

  it should "not return account recent transactions from older blocks and return pending txs" in new TestSetup {
    val blockWithTx = Block(Fixtures.Blocks.Block3125369.signedHeader, Fixtures.Blocks.Block3125369.body)
    blockchain.save(blockWithTx)

    val keyPair = crypto.generateKeyPair(new SecureRandom)

    val tx = Transaction(0, 123, 456, None, 99, ByteString())
    val signedTx = SignedTransaction.sign(tx, keyPair, None)
    val pendingTx = PendingTransaction(signedTx, System.currentTimeMillis)

    val address = signedTx.senderAddress
    val request = GetAccountTransactionsRequest(address, 3125371, 3125381)

    val response = ethService.getAccountTransactions(request)
    pendingTransactionsManager.expectMsg(PendingTransactionsManager.GetPendingTransactions)
    pendingTransactionsManager.reply(PendingTransactionsResponse(Seq(pendingTx)))

    val expectedSent = Seq(TransactionResponse(signedTx, blockHeader = None, pending = Some(true)))

    response.futureValue shouldEqual Right(GetAccountTransactionsResponse(expectedSent, Nil))
  }

  trait TestSetup extends MockFactory with EphemBlockchainTestSetup {
    val blockGenerator = mock[BlockGenerator]
    val appStateStorage = mock[AppStateStorage]
    val keyStore = mock[KeyStore]
    val ledger = mock[Ledger]
    val validators = mock[Validators]
    val blockchainConfig = mock[BlockchainConfig]

    implicit val system = ActorSystem("EthServiceSpec_System")

    val syncingController = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val ommersPool = TestProbe()
    val filterManager = TestProbe()

    val miningConfig = new MiningConfig {
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val activeTimeout: FiniteDuration = Timeouts.shortTimeout
      override val ommerPoolQueryTimeout: FiniteDuration = Timeouts.normalTimeout
      override val headerExtraData: ByteString = ByteString.empty
      override val miningEnabled: Boolean = false
    }

    val filterConfig = new FilterConfig {
      override val filterTimeout: FiniteDuration = Timeouts.normalTimeout
      override val filterManagerQueryTimeout: FiniteDuration = Timeouts.normalTimeout
    }

    val currentProtocolVersion = 11

    val ethService = new EthService(blockchain, blockGenerator, appStateStorage, miningConfig, ledger,
      keyStore, pendingTransactionsManager.ref, syncingController.ref, ommersPool.ref, filterManager.ref, filterConfig, blockchainConfig, currentProtocolVersion)

    val blockToRequest = Block(Fixtures.Blocks.Block3125369.signedHeader, Fixtures.Blocks.Block3125369.body)
    val blockToRequestNumber = blockToRequest.signedHeader.header.number
    val blockToRequestHash = blockToRequest.signedHeader.hash
    val blockTd = blockToRequest.signedHeader.header.difficulty

    val uncle = Fixtures.Blocks.DaoForkBlock.signedHeader
    val uncleTd = uncle.header.difficulty
    val blockToRequestWithUncles = blockToRequest.copy(body = BlockBody(Nil, Seq(uncle)))


    val difficulty = 131072
    val parentBlock = Block(
      signedHeader = SignedBlockHeader(BlockHeader(
        parentHash = ByteString.empty,
        ommersHash = ByteString.empty,
        beneficiary = ByteString.empty,
        stateRoot = ByteString.empty,
        transactionsRoot = ByteString.empty,
        receiptsRoot = ByteString.empty,
        logsBloom = ByteString.empty,
        difficulty = difficulty,
        number = 0,
        gasLimit = 16733003,
        gasUsed = 0,
        unixTimestamp = 1494604900,
        extraData = ByteString.empty,
        mixHash = ByteString.empty,
        nonce = ByteString.empty,
        slotNumber = 0
      ), FakeSignature),
      body = BlockBody(Nil, Nil)
    )
    val block = Block(
      signedHeader = SignedBlockHeader(BlockHeader(
        parentHash = parentBlock.signedHeader.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("000000000000000000000000000000000000002a")),
        stateRoot = ByteString(Hex.decode("2627314387b135a548040d3ca99dbf308265a3f9bd9246bee3e34d12ea9ff0dc")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = difficulty,
        number = 1,
        gasLimit = 16733003,
        gasUsed = 0,
        unixTimestamp = 1494604913,
        extraData = ByteString(Hex.decode("6d696e6564207769746820657463207363616c61")),
        mixHash = ByteString.empty,
        nonce = ByteString.empty,
        slotNumber = 1
      ), FakeSignature),
      body = BlockBody(Nil, Nil)
    )
    val mixHash = ByteString(Hex.decode("40d9bd2064406d7f22390766d6fe5eccd2a67aa89bf218e99df35b2dbb425fb1"))
    val nonce = ByteString(Hex.decode("ce1b500070aeec4f"))
    val seedHash = ByteString(Hex.decode("00" * 32))
    val powHash = ByteString(Hex.decode("533f69824ee25d4f97d61ef9f5251d2dabaf0ccadcdf43484dc02c1ba7fafdee"))
    val target = ByteString((BigInt(2).pow(256) / difficulty).toByteArray)

    val v: Byte = 0x1c
    val r = ByteString(Hex.decode("b3493e863e48a8d67572910933114a4c0e49dac0cb199e01df1575f35141a881"))
    val s = ByteString(Hex.decode("5ba423ae55087e013686f89ad71a449093745f7edb4eb39f30acd30a8964522d"))

    val payload = ByteString(Hex.decode("60606040526040516101e43803806101e483398101604052808051820191906020018051906020019091908051" +
      "9060200190919050505b805b83835b600060018351016001600050819055503373ffffffffffffffffffffffff" +
      "ffffffffffffffff16600260005060016101008110156100025790900160005b50819055506001610102600050" +
      "60003373ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600050819055" +
      "50600090505b82518110156101655782818151811015610002579060200190602002015173ffffffffffffffff" +
      "ffffffffffffffffffffffff166002600050826002016101008110156100025790900160005b50819055508060" +
      "0201610102600050600085848151811015610002579060200190602002015173ffffffffffffffffffffffffff" +
      "ffffffffffffff168152602001908152602001600020600050819055505b80600101905080506100b9565b8160" +
      "00600050819055505b50505080610105600050819055506101866101a3565b610107600050819055505b505b50" +
      "5050602f806101b56000396000f35b600062015180420490506101b2565b905636600080376020600036600073" +
      "6ab9dd83108698b9ca8d03af3c7eb91c0e54c3fc60325a03f41560015760206000f30000000000000000000000" +
      "000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000" +
      "000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000" +
      "0000000000000000000000000000000000000000000000000000020000000000000000000000006c9fbd9a7f06" +
      "d62ce37db2ab1e1b0c288edc797a000000000000000000000000c482d695f42b07e0d6a22925d7e49b46fd9a3f80"))

    //tx 0xb7b8cc9154896b25839ede4cd0c2ad193adf06489fdd9c0a9dfce05620c04ec1
    val contractCreatingTransaction: SignedTransaction = SignedTransaction(Transaction(
      nonce = 2550,
      gasPrice = BigInt("20000000000"),
      gasLimit = 3000000,
      receivingAddress = None,
      value = 0,
      payload
    ), v, r, s, chainId = 0x3d).get

    val fakeReceipt = new Receipt(
      postTransactionStateHash = ByteString(Hex.decode("01" * 32)),
      cumulativeGasUsed = 43,
      logsBloomFilter = ByteString(Hex.decode("00" * 256)),
      logs = Seq(TxLogEntry(Address(42), Seq(ByteString(Hex.decode("01" * 32))), ByteString(Hex.decode("03" * 32)))))

    val createdContractAddress = Address(Hex.decode("c1d93b46be245617e20e75978f5283c889ae048d"))

    val txToRequest = Fixtures.Blocks.Block3125369.body.transactionList.head
    val txToRequestHash = txToRequest.hash
  }

}
