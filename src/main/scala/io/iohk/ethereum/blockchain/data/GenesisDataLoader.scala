package io.iohk.ethereum.blockchain.data

import java.io.{File, FileNotFoundException}

import akka.util.ByteString
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.ByteStringJsonSerializer
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.{crypto, rlp}
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{SignedBlockHeader, _}
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.Ledger.PC
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.vm._
import io.iohk.ethereum.vm.utils.Utils
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JString, JValue}
import org.spongycastle.util.encoders.Hex

import scala.io.Source
import scala.util.{Failure, Success, Try}

class GenesisDataLoader(
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig)
  extends Logger {

  private val bloomLength = 512
  private val hashLength = 64
  private val addressLength = 40

  private val emptyTrieRootHash = ByteString(crypto.kec256(rlp.encode(Array.emptyByteArray)))
  private val emptyEvmHash: ByteString = crypto.kec256(ByteString.empty)

  def loadGenesisData(): Unit = {
    log.debug("Loading genesis data")

    val genesisJson = blockchainConfig.customGenesisFileOpt match {
      case Some(customGenesisFile) =>
        log.debug(s"Trying to load custom genesis data from file: $customGenesisFile")

        Try(Source.fromFile(customGenesisFile)).recoverWith { case _: FileNotFoundException =>
          log.debug(s"Cannot load custom genesis data from file: $customGenesisFile")
          log.debug(s"Trying to load from resources: $customGenesisFile")
          Try(Source.fromResource(customGenesisFile))
        } match {
          case Success(customGenesis) =>
            log.info(s"Using custom genesis data from: $customGenesisFile")
            try {
              customGenesis.getLines().mkString
            } finally {
              customGenesis.close()
            }
          case Failure(ex) =>
            log.error(s"Cannot load custom genesis data from: $customGenesisFile", ex)
            throw ex
        }
      case None =>
        log.info("Using default genesis data")
        val src = Source.fromResource("blockchain/default-genesis.json")
        try {
          src.getLines().mkString
        } finally {
          src.close()
        }
    }

    loadGenesisData(genesisJson) match {
      case Success(_) =>
        log.info("Genesis data successfully loaded")
      case Failure(ex) =>
        log.error("Unable to load genesis data", ex)
        throw ex
    }
  }

  private def loadGenesisData(genesisJson: String): Try[Unit] = {
    import org.json4s.native.JsonMethods.parse
    implicit val formats: Formats = DefaultFormats + ByteStringJsonSerializer
    for {
      genesisData <- Try(parse(genesisJson).extract[GenesisData])
      _ <- loadGenesisData(genesisData)
    } yield ()
  }

  // scalastyle:off method.length
  private def loadGenesisData(genesisData: GenesisData): Try[Unit] = {
    import MerklePatriciaTrie.defaultByteArraySerializable

    val ephemDataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(ephemDataSource)
    val initalRootHash = MerklePatriciaTrie.EmptyRootHash

    log.debug("Start saving pre-defined accounts")
    val stateMptRootHash = genesisData.alloc.zipWithIndex.foldLeft(initalRootHash) {
      case (rootHash, (((address, AllocAccount(balance)), idx))) => {
        val ephemNodeStorage = pruning.PruningMode.nodesKeyValueStorage(pruning.ArchivePruning, nodeStorage)(Some(idx - genesisData.alloc.size))
        val mpt = MerklePatriciaTrie[Array[Byte], Account](rootHash, ephemNodeStorage)
        val paddedAddress = address.reverse.padTo(addressLength, "0").reverse.mkString
        mpt.put(crypto.kec256(Hex.decode(paddedAddress)),
          Account(blockchainConfig.accountStartNonce, UInt256(BigInt(balance)), emptyTrieRootHash, emptyEvmHash)
        ).getRootHash
      }
    }

    val preBlockHeader: BlockHeader = prepareGenesisBlockHeader(genesisData, stateMptRootHash)
    log.debug(s"Prepared genesis header: $preBlockHeader")

    ephemDataSource.getAll(nodeStorage.namespace)
      .foreach { case (key, value) => blockchain.saveNode(ByteString(key.toArray[Byte]), value.toArray[Byte], 0) }

    val contractAddress = Address("0x000000000000000000000000000000000000000a")

    // TODO: Get path and contract by config param
    log.debug("Start loading pre-defined consensus contract")
    val ContractDir = new File("target/contracts")
    val contractInitCode = Utils.loadContractCodeFromFile(new File(s"$ContractDir/Fibonacci.bin"))
    val contractAbi = Utils.loadContractAbiFromFile(new File(s"$ContractDir/Fibonacci.abi"))

    log.debug("Pre-defined consensus contract found")
    val fromAddress = Address("0x0000000000000000000000000000000000000000")
    val dummySignature = ECDSASignature(0, 0, 0.toByte)
    val tx = Transaction(
      nonce = BigInt(1),
      gasPrice = 0,
      gasLimit = 90000000, // TODO: get value from config param?
      receivingAddress = None,
      value = BigInt(0),
      payload = contractInitCode
    )
    val stx = SignedTransaction(tx, dummySignature, fromAddress)
    val evmConfig = EvmConfig.forBlock(blockNumber = BigInt(0), blockchainConfig)
    val worldWithContractAccount = blockchain
      .getWorldStateProxy(BigInt(0), blockchainConfig.accountStartNonce, Some(ByteString(stateMptRootHash)))
      .saveAccount(contractAddress, Account())
    val context: PC = ProgramContext(stx, contractAddress, Program(stx.tx.payload), preBlockHeader, worldWithContractAccount, evmConfig)
    val result = VM.run(context)
    val worldWithCode = result.world.saveCode(contractAddress, result.returnData)
    val lastRootHash = InMemoryWorldStateProxy.persistState(worldWithCode).stateRootHash

    // TODO: throw a failure if an error occured
    log.debug(s"Error while simulate contract tx: ${result.error.toString}")

    val signedHeader = prepareGenesisSignedBlockHeader(preBlockHeader, lastRootHash)

    blockchain.getSignedBlockHeaderByNumber(0) match {
      case Some(existingSignedGenesisHeader) if existingSignedGenesisHeader.hash == signedHeader.hash =>
        log.debug("Genesis data already in the database")
        Success(())
      case Some(_) =>
        Failure(new RuntimeException("Genesis data present in the database does not match genesis block from file." +
          " Use different directory for running private blockchains."))
      case None =>
        blockchain.save(Block(signedHeader, BlockBody(Nil, Nil)), Nil, signedHeader.header.difficulty, saveAsBestBlock = true)
        Success(())
    }
  }


  private def prepareGenesisBlockHeader(genesisData: GenesisData, stateMptRootHash: Array[Byte]) = {
    BlockHeader(
      parentHash = zeros(hashLength),
      ommersHash = ByteString(crypto.kec256(rlp.encode(RLPList()))),
      beneficiary = genesisData.coinbase,
      stateRoot = ByteString(stateMptRootHash),
      transactionsRoot = emptyTrieRootHash,
      receiptsRoot = emptyTrieRootHash,
      logsBloom = zeros(bloomLength),
      difficulty = BigInt(genesisData.difficulty.replace("0x", ""), 16),
      number = 0,
      gasLimit = BigInt(genesisData.gasLimit.replace("0x", ""), 16),
      gasUsed = 0,
      unixTimestamp = BigInt(genesisData.timestamp.replace("0x", ""), 16).toLong,
      extraData = genesisData.extraData,
      mixHash = genesisData.mixHash.getOrElse(zeros(hashLength)),
      nonce = genesisData.nonce,
      slotNumber = 0)
  }

  private def prepareGenesisSignedBlockHeader(prepareGenesisBlockHeader: BlockHeader, stateMptRootHash: ByteString) = {
    val genesisSignature = ECDSASignature(
      s = zeros(ECDSASignature.SLength),
      r = zeros(ECDSASignature.SLength),
      v = 0.toByte)
    SignedBlockHeader(prepareGenesisBlockHeader.copy(stateRoot = stateMptRootHash), genesisSignature)
  }

  private def zeros(length: Int) =
    ByteString(Hex.decode(List.fill(length)("0").mkString))

}

object GenesisDataLoader {

  object JsonSerializers {

    def deserializeByteString(jv: JValue): ByteString = jv match {
      case JString(s) =>
        val noPrefix = s.replace("0x", "")
        val inp =
          if (noPrefix.length % 2 == 0) noPrefix
          else "0" ++ noPrefix
        Try(ByteString(Hex.decode(inp))) match {
          case Success(bs) => bs
          case Failure(_) => throw new RuntimeException("Cannot parse hex string: " + s)
        }
      case other => throw new RuntimeException("Expected hex string, but got: " + other)
    }

    object ByteStringJsonSerializer extends CustomSerializer[ByteString](formats =>
      (
        { case jv => deserializeByteString(jv) },
        PartialFunction.empty
      )
    )

  }
}
