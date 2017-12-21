package io.iohk.ethereum.blockchain.data

import java.io.{File, FileNotFoundException}

import akka.util.ByteString
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.ContractData
import io.iohk.ethereum.blockchain.data.GenesisDataLoader.JsonSerializers.ByteStringJsonSerializer
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, OuroborosConfig}
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
    ouroborosConfig: OuroborosConfig,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    virtualMachine: VM)
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
      consensusContractCode <- Try(Utils.loadContractCodeFromFile(new File(ouroborosConfig.consensusContractFilepath)))
      _ <- loadGenesisData(genesisData, ContractData(ouroborosConfig.consensusContractAddress, consensusContractCode))
    } yield ()
  }

  private def loadGenesisData(genesisData: GenesisData, consensusContractData: ContractData): Try[Unit] = {
    val ephemDataSource = EphemDataSource()
    val nodeStorage = new NodeStorage(ephemDataSource)
    val initialRootHash = MerklePatriciaTrie.EmptyRootHash
    val stateMptRootHash = ByteString(storeUserAllocAccounts(nodeStorage, genesisData, initialRootHash))

    log.debug("About to save user alloc accounts")
    ephemDataSource.getAll(nodeStorage.namespace)
      .foreach { case (key, value) => blockchain.saveNode(ByteString(key.toArray[Byte]), value.toArray[Byte], 0) }

    log.debug("About to deploy consensus contract")
    val preBlockHeader: BlockHeader = prepareGenesisBlockHeader(genesisData, stateMptRootHash)
    // FIXME: Manage error situations
    val lastRootHash = deployConsensusContract(consensusContractData, preBlockHeader, stateMptRootHash)

    log.debug("About to save genesis block")
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

  private def storeUserAllocAccounts(nodeStorage: NodeStorage, genesisData: GenesisData, initialRootHash: Array[Byte]): Array[Byte] = {
    import MerklePatriciaTrie.defaultByteArraySerializable
    genesisData.alloc.zipWithIndex.foldLeft(initialRootHash) {
      case (rootHash, (((address, AllocAccount(balance)), idx))) =>
        val ephemNodeStorage = pruning.PruningMode.nodesKeyValueStorage(pruning.ArchivePruning, nodeStorage)(Some(idx - genesisData.alloc.size))
        val mpt = MerklePatriciaTrie[Array[Byte], Account](rootHash, ephemNodeStorage)
        val paddedAddress = address.reverse.padTo(addressLength, "0").reverse.mkString
        mpt.put(crypto.kec256(Hex.decode(paddedAddress)),
        Account(blockchainConfig.accountStartNonce, UInt256(BigInt(balance)), emptyTrieRootHash, emptyEvmHash)
      ).getRootHash
    }
  }

  private def deployConsensusContract(consensusContractData: ContractData, preBlockHeader: BlockHeader, currentStateRootHash: ByteString): ByteString = {
    val stx = createSignedContractTransaction(consensusContractData.code)
    val evmConfig = EvmConfig.forBlock(blockNumber = BigInt(0), blockchainConfig)
    val worldWithContractAccount = blockchain
      .getWorldStateProxy(BigInt(0), blockchainConfig.accountStartNonce, Some(currentStateRootHash))
      .saveAccount(consensusContractData.address, Account())
    val context: PC = ProgramContext(stx, consensusContractData.address, Program(stx.tx.payload), preBlockHeader, worldWithContractAccount, evmConfig)
    val result = virtualMachine.run(context)
    log.debug(s"Error while deploy consensus contract: ${result.error.toString}")
    val worldWithCode = result.world.saveCode(consensusContractData.address, result.returnData)
    InMemoryWorldStateProxy.persistState(worldWithCode).stateRootHash
  }

  // FIXME: Gas limit should be set from config param?
  // scalastyle:off magic.number
  private def createSignedContractTransaction(contractCode: ByteString): SignedTransaction = {
    val fromAddress = Address("0x0000000000000000000000000000000000000000")
    val dummySignature = ECDSASignature(0, 0, 0.toByte)
    val tx = Transaction(
      nonce = BigInt(1),
      gasPrice = 0,
      gasLimit = 90000000,
      receivingAddress = None,
      value = BigInt(0),
      payload = contractCode
    )
    SignedTransaction(tx, dummySignature, fromAddress)
  }

  private def prepareGenesisBlockHeader(genesisData: GenesisData, currentStateRootHash: ByteString) = {
    BlockHeader(
      parentHash = zeros(hashLength),
      ommersHash = ByteString(crypto.kec256(rlp.encode(RLPList()))),
      beneficiary = genesisData.coinbase,
      stateRoot = currentStateRootHash,
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

  private def prepareGenesisSignedBlockHeader(prepareGenesisBlockHeader: BlockHeader, currentStateRootHash: ByteString) = {
    val genesisSignature = ECDSASignature(
      s = zeros(ECDSASignature.SLength),
      r = zeros(ECDSASignature.SLength),
      v = 0.toByte)
    SignedBlockHeader(prepareGenesisBlockHeader.copy(stateRoot = currentStateRootHash), genesisSignature)
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
      ( {
        case jv => deserializeByteString(jv)
      },
        PartialFunction.empty
      )
    )
  }

  case class ContractData(address: Address, code: ByteString)
}
