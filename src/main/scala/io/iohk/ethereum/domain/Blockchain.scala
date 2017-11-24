package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode, NodesKeyValueStorage}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._

/**
  * Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
// scalastyle:off number.of.methods
trait Blockchain {

  type S <: Storage[S]
  type WS <: WorldStateProxy[WS, S]

  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getSignedBlockHeaderByHash(hash: ByteString): Option[SignedBlockHeader]

  def getSignedBlockHeaderByNumber(number: BigInt): Option[SignedBlockHeader] = {
    for {
      hash <- getHashByBlockNumber(number)
      header <- getSignedBlockHeaderByHash(hash)
    } yield header
  }

  /**
    * Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[io.iohk.ethereum.network.p2p.messages.PV62.BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody]

  /**
    * Allows to query for a block based on it's hash
    *
    * @param hash of the block that's being searched
    * @return Block if found
    */
  def getBlockByHash(hash: ByteString): Option[Block] =
    for {
      header <- getSignedBlockHeaderByHash(hash)
      body <- getBlockBodyByHash(hash)
    } yield Block(header, body)

  /**
    * Allows to query for a block based on it's number
    *
    * @param number Block number
    * @return Block if it exists
    */
  def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /**
    * Get an account for an address and a block number
    *
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(address: Address, blockNumber: BigInt): Option[Account]

  /**
    * Get account storage at given position
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getAccountStorageAt(rootHash: ByteString, position: BigInt): ByteString

  /**
    * Returns the receipts based on a block hash
    * @param blockhash
    * @return Receipts if found
    */
  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]]

  /**
    * Returns EVM code searched by it's hash
    * @param hash Code Hash
    * @return EVM code if found
    */
  def getEvmCodeByHash(hash: ByteString): Option[ByteString]

  /**
    * Returns MPT node searched by it's hash
    * @param hash Node Hash
    * @return MPT node
    */
  def getMptNodeByHash(hash: ByteString): Option[MptNode]

  /**
    * Returns the total difficulty based on a block hash
    * @param blockhash
    * @return total difficulty if found
    */
  def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt]

  def getTotalDifficultyByNumber(blockNumber: BigInt): Option[BigInt] =
    getHashByBlockNumber(blockNumber).flatMap(getTotalDifficultyByHash)

  def getTransactionLocation(txHash: ByteString): Option[TransactionLocation]

  def getBestBlockNumber(): BigInt

  def getBestBlock(): Block


  /**
    * Persists full block along with receipts and total difficulty
    * @param saveAsBestBlock - whether to save the block's number as current best block
    */
  def save(block: Block, receipts: Seq[Receipt], totalDifficulty: BigInt, saveAsBestBlock: Boolean): Unit = {
    save(block)
    save(block.signedHeader.hash, receipts)
    save(block.signedHeader.hash, totalDifficulty)
    if (saveAsBestBlock) {
      saveBestBlockNumber(block.signedHeader.header.number)
    }
    pruneState(block.signedHeader.header.number)
  }

  /**
    * Persists a block in the underlying Blockchain Database
    *
    * @param block Block to be saved
    */
  def save(block: Block): Unit = {
    save(block.signedHeader)
    save(block.signedHeader.hash, block.body)
  }

  def removeBlock(hash: ByteString, saveParentAsBestBlock: Boolean): Unit

  /**
    * Persists a block header in the underlying Blockchain Database
    *
    * @param signedBlockHeader Block to be saved
    */
  def save(signedBlockHeader: SignedBlockHeader): Unit

  def save(blockHash: ByteString, blockBody: BlockBody): Unit

  def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit

  def save(hash: ByteString, evmCode: ByteString): Unit

  def save(blockhash: ByteString, totalDifficulty: BigInt): Unit

  def saveBestBlockNumber(number: BigInt): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  protected def getHashByBlockNumber(number: BigInt): Option[ByteString]

  def genesisSignedHeader: SignedBlockHeader = getSignedBlockHeaderByNumber(0).get

  def genesisBlock: Block = getBlockByNumber(0).get

  def getWorldStateProxy(blockNumber: BigInt,
                         accountStartNonce: UInt256,
                         stateRootHash: Option[ByteString] = None,
                         noEmptyAccounts: Boolean = false): WS

  def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt],
                                 accountStartNonce: UInt256,
                                 stateRootHash: Option[ByteString] = None,
                                 noEmptyAccounts: Boolean = false): WS

  def pruneState(blockNumber: BigInt): Unit

  def rollbackStateChangesMadeByBlock(blockNumber: BigInt): Unit
}
// scalastyle:on

class BlockchainImpl(
                      protected val blockHeadersStorage: SignedBlockHeadersStorage,
                      protected val blockBodiesStorage: BlockBodiesStorage,
                      protected val blockNumberMappingStorage: BlockNumberMappingStorage,
                      protected val receiptStorage: ReceiptStorage,
                      protected val evmCodeStorage: EvmCodeStorage,
                      protected val pruningMode: PruningMode,
                      protected val nodeStorage: NodeStorage,
                      protected val totalDifficultyStorage: TotalDifficultyStorage,
                      protected val transactionMappingStorage: TransactionMappingStorage,
                      protected val appStateStorage: AppStateStorage
) extends Blockchain {

  override def getSignedBlockHeaderByHash(hash: ByteString): Option[SignedBlockHeader] =
    blockHeadersStorage.get(hash)

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)

  override def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt] = totalDifficultyStorage.get(blockhash)

  override def getBestBlockNumber(): BigInt =
    appStateStorage.getBestBlockNumber()

  override def getBestBlock(): Block =
    getBlockByNumber(getBestBlockNumber()).get

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getSignedBlockHeaderByNumber(blockNumber).flatMap { sbh =>
      val mpt = MerklePatriciaTrie[Address, Account](
        sbh.header.stateRoot.toArray,
        nodesKeyValueStorageFor(Some(blockNumber))
      )
      mpt.get(address)
    }

  override def getAccountStorageAt(rootHash: ByteString, position: BigInt): ByteString = {
    storageMpt(
      rootHash,
      nodesKeyValueStorageFor(None)
    ).get(UInt256(position)).getOrElse(UInt256(0)).bytes
  }

  override def save(signedBlockHeader: SignedBlockHeader): Unit = {
    val hash = signedBlockHeader.hash
    blockHeadersStorage.put(hash, signedBlockHeader)
    saveBlockNumberMapping(signedBlockHeader.header.number, hash)
  }

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] = nodesKeyValueStorageFor(None).get(hash).map(_.toMptNode)

  override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] = transactionMappingStorage.get(txHash)

  override def save(blockHash: ByteString, blockBody: BlockBody): Unit = {
    blockBodiesStorage.put(blockHash, blockBody)
    saveTxsLocations(blockHash, blockBody)
  }

  override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = receiptStorage.put(blockHash, receipts)

  override def save(hash: ByteString, evmCode: ByteString): Unit = evmCodeStorage.put(hash, evmCode)

  override def saveBestBlockNumber(number: BigInt): Unit =
    appStateStorage.putBestBlockNumber(number)

  def save(blockhash: ByteString, td: BigInt): Unit = totalDifficultyStorage.put(blockhash, td)

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit =
    nodesKeyValueStorageFor(Some(blockNumber)).put(nodeHash, nodeEncoded)

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): Unit =
    blockNumberMappingStorage.put(number, hash)

  private def removeBlockNumberMapping(number: BigInt): Unit = {
    blockNumberMappingStorage.remove(number)
    appStateStorage.putBestBlockNumber(number - 1) // FIXME: mother of consistency?!?!
  }

  override def removeBlock(blockHash: ByteString, saveParentAsBestBlock: Boolean): Unit = {
    val maybeSignedBlockHeader = getSignedBlockHeaderByHash(blockHash)
    val maybeTxList = getBlockBodyByHash(blockHash).map(_.transactionList)

    blockHeadersStorage.remove(blockHash)
    blockBodiesStorage.remove(blockHash)
    totalDifficultyStorage.remove(blockHash)
    receiptStorage.remove(blockHash)
    maybeTxList.foreach(removeTxsLocations)
    maybeSignedBlockHeader.foreach{ sbh =>
      rollbackStateChangesMadeByBlock(sbh.header.number)
      if (getHashByBlockNumber(sbh.header.number).contains(blockHash))
        removeBlockNumberMapping(sbh.header.number)

      if (saveParentAsBestBlock) {
        appStateStorage.putBestBlockNumber(sbh.header.number - 1)
      }
    }
  }

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): Unit =
    blockBody.transactionList.zipWithIndex.foreach{ case (tx, index) =>
      transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)) }

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): Unit = {
    stxs.map(_.hash).foreach{ transactionMappingStorage.remove }
  }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(blockNumber: BigInt,
                                  accountStartNonce: UInt256,
                                  stateRootHash: Option[ByteString],
                                  noEmptyAccount: Boolean = false): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      nodesKeyValueStorageFor(Some(blockNumber)),
      accountStartNonce,
      (number: BigInt) => getSignedBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccount
    )

  //FIXME Maybe we can use this one in regular execution too and persist underlying storage when block execution is successful
  override def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt],
                                          accountStartNonce: UInt256,
                                          stateRootHash: Option[ByteString],
                                          noEmptyAccount: Boolean = false): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      ReadOnlyNodeStorage(nodesKeyValueStorageFor(blockNumber)),
      accountStartNonce,
      (number: BigInt) => getSignedBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts = false
    )

  def nodesKeyValueStorageFor(blockNumber: Option[BigInt]): NodesKeyValueStorage =
    PruningMode.nodesKeyValueStorage(pruningMode, nodeStorage)(blockNumber)

  def pruneState(blockNumber: BigInt): Unit = PruningMode.prune(pruningMode, blockNumber, nodeStorage)

  def rollbackStateChangesMadeByBlock(blockNumber: BigInt): Unit = PruningMode.rollback(pruningMode, blockNumber, nodeStorage)
}

trait BlockchainStorages {
  val blockHeadersStorage: SignedBlockHeadersStorage
  val blockBodiesStorage: BlockBodiesStorage
  val blockNumberMappingStorage: BlockNumberMappingStorage
  val receiptStorage: ReceiptStorage
  val evmCodeStorage: EvmCodeStorage
  val totalDifficultyStorage: TotalDifficultyStorage
  val transactionMappingStorage: TransactionMappingStorage
  val nodeStorage: NodeStorage
  val pruningMode: PruningMode
  val appStateStorage: AppStateStorage
}

object BlockchainImpl {
  def apply(storages: BlockchainStorages): BlockchainImpl =
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      evmCodeStorage = storages.evmCodeStorage,
      pruningMode = storages.pruningMode,
      nodeStorage = storages.nodeStorage,
      totalDifficultyStorage = storages.totalDifficultyStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage
    )
}
