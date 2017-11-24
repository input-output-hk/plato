package io.iohk.ethereum.mining

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger.{BlockPreparationResult, BlockResult}
import io.iohk.ethereum.ledger.{BlockPreparationError, BloomFilter, InvalidBlockHeaderSignature, Ledger}
import io.iohk.ethereum.mpt.{ByteArraySerializable, MerklePatriciaTrie}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV62.SignedBlockHeaderImplicits._
import io.iohk.ethereum.utils.{BlockchainConfig, MiningConfig}
import io.iohk.ethereum.utils.ByteUtils.or
import io.iohk.ethereum.validators.MptListValidator.intByteArraySerializable
import io.iohk.ethereum.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.validators.Validators
import io.iohk.ethereum.crypto._

class BlockGenerator(blockchain: Blockchain, blockchainConfig: BlockchainConfig, miningConfig: MiningConfig,
                     ledger: Ledger, validators: Validators, blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider, blockHeaderSignerFn: (BlockHeader, Address) => Option[SignedBlockHeader]) {

  val difficulty = new DifficultyCalculator(blockchainConfig)

  private val cache: AtomicReference[List[PendingBlock]] = new AtomicReference(Nil)

  def generateBlockForMining(parent: Block, transactions: Seq[SignedTransaction], ommers: Seq[SignedBlockHeader], beneficiary: Address, slotNumber: BigInt):
  Either[BlockPreparationError, PendingBlock] = {
    val blockNumber = parent.signedHeader.header.number + 1
    val parentHash = parent.signedHeader.hash
    val blockTimestamp = blockTimestampProvider.getEpochSecond
    val preHeader = prepareHeader(blockNumber, ommers, beneficiary, parent, blockTimestamp, slotNumber)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val preSignedHeader = SignedBlockHeader(preHeader, fakeSignature)
    val transactionsForBlock: List[SignedTransaction] = prepareTransactions(transactions, preSignedHeader.header.gasLimit)
    val body = BlockBody(transactionsForBlock, ommers)
    val preBlock = Block(preSignedHeader, body)
    val result = ledger.prepareBlock(preBlock) match {
      case BlockPreparationResult(prepareBlock, BlockResult(_, gasUsed, receipts), stateRoot) =>
        val receiptsLogs: Seq[Array[Byte]] = BloomFilter.EmptyBloomFilter.toArray +: receipts.map(_.logsBloomFilter.toArray)
        val bloomFilter = ByteString(or(receiptsLogs: _*))
        val header = preBlock.signedHeader.header.copy(
          transactionsRoot = buildMpt(prepareBlock.body.transactionList, SignedTransaction.byteArraySerializable),
          stateRoot = stateRoot,
          receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
          logsBloom = bloomFilter,
          gasUsed = gasUsed)
          blockHeaderSignerFn(header, beneficiary).toRight(InvalidBlockHeaderSignature).map(
          signedBlockHeader => PendingBlock(Block(signedBlockHeader, prepareBlock.body), receipts)
        )
    }
    result.foreach(b => cache.updateAndGet(new UnaryOperator[List[PendingBlock]] {
      override def apply(t: List[PendingBlock]): List[PendingBlock] =
        (b :: t).take(miningConfig.blockCacheSize)
    }))
    result
  }

  private def prepareTransactions(transactions: Seq[SignedTransaction], blockGasLimit: BigInt) = {
    val sortedTransactions = transactions.groupBy(_.senderAddress).values.toList.flatMap { txsFromSender =>
      val ordered = txsFromSender
        .sortBy(-_.tx.gasPrice)
        .sortBy(_.tx.nonce)
        .foldLeft(Seq.empty[SignedTransaction]) { case (txs, tx) =>
          if (txs.exists(_.tx.nonce == tx.tx.nonce)) {
            txs
          } else {
            txs :+ tx
          }
        }
        .takeWhile(_.tx.gasLimit <= blockGasLimit)
      ordered.headOption.map(_.tx.gasPrice -> ordered)
    }.sortBy { case (gasPrice, _) => gasPrice }.reverse.flatMap { case (_, txs) => txs }

    val transactionsForBlock = sortedTransactions
      .scanLeft(BigInt(0), None: Option[SignedTransaction]) { case ((accumulatedGas, _), stx) => (accumulatedGas + stx.tx.gasLimit, Some(stx)) }
      .collect { case (gas, Some(stx)) => (gas, stx) }
      .takeWhile { case (gas, _) => gas <= blockGasLimit }
      .map { case (_, stx) => stx }
    transactionsForBlock
  }

  private def prepareHeader(blockNumber: BigInt,
                            ommers: Seq[SignedBlockHeader],
                            beneficiary: Address,
                            parent: Block,
                            blockTimestamp: Long,
                            slotNumber: BigInt) = {
    import blockchainConfig.daoForkConfig

    BlockHeader(
      parentHash = parent.signedHeader.hash,
      ommersHash = ByteString(kec256(ommers.toBytes: Array[Byte])),
      beneficiary = beneficiary.bytes,
      stateRoot = ByteString.empty,
      //we are not able to calculate transactionsRoot here because we do not know if they will fail
      transactionsRoot = ByteString.empty,
      receiptsRoot = ByteString.empty,
      logsBloom = ByteString.empty,
      difficulty = difficulty.calculateDifficulty(blockNumber, blockTimestamp, parent.signedHeader.header),
      number = blockNumber,
      gasLimit = calculateGasLimit(parent.signedHeader.header.gasLimit),
      gasUsed = 0,
      unixTimestamp = blockTimestamp,
      extraData = daoForkConfig.flatMap(daoForkConfig => daoForkConfig.getExtraData(blockNumber)).getOrElse(miningConfig.headerExtraData),
      mixHash = ByteString.empty,
      nonce = ByteString.empty,
      slotNumber = slotNumber
    )
  }

  /**
    * This function returns the block currently being mined block with highest timestamp
    */
  def getPending: Option[PendingBlock] = {
    val pendingBlocks = cache.get()
    if(pendingBlocks.isEmpty) None
    else Some(pendingBlocks.maxBy(_.block.signedHeader.header.unixTimestamp))
  }

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: BigInt): BigInt = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  private def buildMpt[K](entities: Seq[K], vSerializable: ByteArraySerializable[K]): ByteString = {
    val mpt = MerklePatriciaTrie[Int, K](
      source = new ArchiveNodeStorage(new NodeStorage(EphemDataSource()))
    )(intByteArraySerializable, vSerializable)
    val hash = entities.zipWithIndex.foldLeft(mpt) { case (trie, (value, key)) => trie.put(key, value) }.getRootHash
    ByteString(hash)
  }

}

trait BlockTimestampProvider {
  def getEpochSecond: Long
}

case class PendingBlock(block: Block, receipts: Seq[Receipt])

object DefaultBlockTimestampProvider extends BlockTimestampProvider {
  override def getEpochSecond: Long = Instant.now.getEpochSecond
}

object BlockGenerator {

  case class InvalidOmmers(reason: OmmersError) extends BlockPreparationError

}
