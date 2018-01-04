package io.iohk.ethereum.mining

import java.time.Instant
import akka.util.ByteString
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.{Timeouts, crypto}
import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparationError, LedgerImpl}
import io.iohk.ethereum.utils._
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.SignedTransaction.FirstByteOfAddress
import io.iohk.ethereum.utils.Config.SyncConfig
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import scala.concurrent.duration.FiniteDuration
import io.iohk.ethereum.Fixtures.FakeSignature
import io.iohk.ethereum.blockchain.EphemBlockchainTestSetup
import scala.concurrent.duration._


class BlockGeneratorSpec extends FlatSpec with Matchers with PropertyChecks with Logger {

  "BlockGenerator" should "generate correct block with empty transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] = blockGenerator.generateBlockForMining(bestBlock, Nil, Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("eb49a2da108d63de"))
    val minedMixHash = ByteString(Hex.decode("a91c44e62d17005c4b22f6ed116f485ea30d8b63f2429745816093b304eb4f73"))
    val miningTimestamp = 1508751768

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "generate correct block with transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] = blockGenerator.generateBlockForMining(bestBlock, Seq(signedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("4139b957dae0488d"))
    val minedMixHash = ByteString(Hex.decode("dc25764fb562d778e5d1320f4c3ba4b09021a2603a0816235e16071e11f342ea"))
    val miningTimestamp = 1508752265

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
        val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
        val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
        pb.block.copy(signedHeader = signerHeader)
      })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "filter out failing transactions" in new TestSetup {
    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(signedTransaction, duplicatedSignedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("12cb47f9208d1e81"))
    val minedMixHash = ByteString(Hex.decode("908471b57f2d3e70649f9ce0c9c318d61146d3ce19f70d2f94309f135b87b64a"))
    val miningTimestamp = 1508752389

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "filter out transactions exceeding block gas limit and include correct transactions" in new TestSetup {
    val txWitGasTooBigGasLimit: SignedTransaction = SignedTransaction.sign(
      transaction.copy(
        gasLimit = BigInt(2).pow(100000),
        nonce = signedTransaction.tx.nonce + 1),
      keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(txWitGasTooBigGasLimit, signedTransaction, duplicatedSignedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("38026e10fb18b458"))
    val minedMixHash = ByteString(Hex.decode("806f26f0efb12a0c0c16e587984227186c46f25fc4e76698a68996183edf2cf1"))
    val miningTimestamp = 1508752492

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map (pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "generate block before eip155 and filter out chain specific tx" in new TestSetup {
    override lazy val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val eip155BlockNumber: BigInt = Long.MaxValue
      override val eip106BlockNumber: BigInt = Long.MaxValue
      override val chainId: Byte = 0x3d.toByte
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))

      // unused
      override val maxCodeSize: Option[BigInt] = None
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val eip161BlockNumber: BigInt = Long.MaxValue
      override val accountStartNonce: UInt256 = UInt256.Zero
      override val daoForkConfig: Option[DaoForkConfig] = None
      val gasTieBreaker: Boolean = false
    }

    val generalTx = SignedTransaction.sign(transaction, keyPair, None)
    val specificTx = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(generalTx, specificTx), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("48381cb0cd40936a"))
    val minedMixHash = ByteString(Hex.decode("dacd96cf5dbc662fa113c73319fcdc7d6e7053571432345b936fd221c1e18d42"))
    val miningTimestamp = 1499952002

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(generalTx))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "generate block after eip155 and allow both chain specific and general transactions" in new TestSetup {
    val generalTx: SignedTransaction = SignedTransaction.sign(transaction.copy(nonce = transaction.nonce + 1), keyPair, None)

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(generalTx, signedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("39bd50fcbde30b18"))
    val minedMixHash = ByteString(Hex.decode("c77dae7cef6c685896ed6b8026466a2e6338b8bc5f182e2dd7a64cf7da9c7d1b"))
    val miningTimestamp = 1499951223

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction, generalTx))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "include consecutive transactions from single sender" in new TestSetup {
    val nextTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.nonce + 1), keyPair, Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(nextTransaction, signedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction, nextTransaction))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "filter out failing transaction from the middle of tx list" in new TestSetup {
    val nextTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(nonce = signedTransaction.tx.nonce + 1), keyPair, Some(0x3d.toByte))

    val privateKeyWithNoEthere = BigInt(1, Hex.decode("584a31be275195585603ddd05a53d16fae9deafba67213b6060cec9f16e44cae"))

    val failingTransaction = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty)
    val signedFailingTransaction: SignedTransaction = SignedTransaction.sign(failingTransaction,
      keyPairFromPrvKey(privateKeyWithNoEthere), Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(nextTransaction, signedFailingTransaction, signedTransaction),
        Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("8f88ec20f1be482f"))
    val minedMixHash = ByteString(Hex.decode("247a206abc088487edc1697fcaceb33ad87b55666e438129b7048bb08c8ed88f"))
    val miningTimestamp = 1499721182

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction, nextTransaction))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  it should "include transaction with higher gas price if nonce is the same" in new TestSetup {
    val txWitSameNonceButLowerGasPrice: SignedTransaction = SignedTransaction.sign(
      transaction.copy(gasPrice = signedTransaction.tx.gasPrice - 1),
      keyPair,
      Some(0x3d.toByte))

    val result: Either[BlockPreparationError, PendingBlock] =
      blockGenerator.generateBlockForMining(bestBlock, Seq(txWitSameNonceButLowerGasPrice, signedTransaction), Nil, Address(testAddress), slotNumber = -1)
    result shouldBe a[Right[_, Block]]

    //mined with mantis + ethminer
    val minedNonce = ByteString(Hex.decode("14d7000ac544b38e"))
    val minedMixHash = ByteString(Hex.decode("270f6b2618c5bef6a188397927129c803e5fd41c85492835486832f6825a8d78"))
    val miningTimestamp = 1508752698

    val fullBlock: Either[BlockPreparationError, Block] = result.right.map(pb => {
      val blockHeader = pb.block.signedHeader.header.copy(nonce = minedNonce, mixHash = minedMixHash, unixTimestamp = miningTimestamp)
      val signerHeader = pb.block.signedHeader.copy(header = blockHeader)
      pb.block.copy(signedHeader = signerHeader)
    })
    fullBlock.right.foreach(b => validators.blockHeaderValidator.validate(b.signedHeader, blockchain) shouldBe Right(BlockHeaderValid))
    fullBlock.right.foreach(b => ledger.executeBlock(b) shouldBe a[Right[_, Seq[Receipt]]])
    fullBlock.right.foreach(b => b.body.transactionList shouldBe Seq(signedTransaction))
    fullBlock.right.foreach(b => b.signedHeader.header.extraData shouldBe miningConfig.headerExtraData)
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val testAddress = 42
    val privateKey = BigInt(1, Hex.decode("f3202185c84325302d43887e90a2e23e7bc058d0450bb58ef2f7585765d7d48b"))
    lazy val keyPair: AsymmetricCipherKeyPair = keyPairFromPrvKey(privateKey)
    lazy val pubKey: Array[Byte] = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    lazy val address = Address(crypto.kec256(pubKey).drop(FirstByteOfAddress))

    val txGasLimit = 21000
    val txTransfer = 9000
    val transaction = Transaction(
      nonce = 0,
      gasPrice = 1,
      gasLimit = txGasLimit,
      receivingAddress = Address(testAddress),
      value = txTransfer,
      payload = ByteString.empty)
    lazy val signedTransaction: SignedTransaction = SignedTransaction.sign(transaction, keyPair, Some(0x3d.toByte))
    lazy val duplicatedSignedTransaction: SignedTransaction = SignedTransaction.sign(transaction.copy(gasLimit = 2), keyPair, Some(0x3d.toByte))

    lazy val blockchainConfig = new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val eip155BlockNumber: BigInt = 0
      override val eip106BlockNumber: BigInt = Long.MaxValue
      override val chainId: Byte = 0x3d.toByte
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))

      // unused
      override val maxCodeSize: Option[BigInt] = None
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val eip161BlockNumber: BigInt = Long.MaxValue
      override val accountStartNonce: UInt256 = UInt256.Zero
      override val daoForkConfig: Option[DaoForkConfig] = None
      val gasTieBreaker: Boolean = false
    }

    val syncConfig = SyncConfig(Config.config)

    lazy val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = MockValidatorsAlwaysSucceed.blockHeaderValidator
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
    }

    lazy val ledger = new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, validators)

    val ouroborosConfig = new OuroborosConfig {
      override val consensusContractFilepath: String = "src/test/resources/CertificateAuthorityManager"
      // unused
      override val consensusContractAddress: Address = Address(0)
      override val initialCA: Address = Address(1)
      override val consensusApprovalPercentage = 0
      override val slotDuration: FiniteDuration = 0.millis
    }

    val genesisDataLoader = new GenesisDataLoader(ouroborosConfig, blockchain, blockchainConfig, VM)
    genesisDataLoader.loadGenesisData()

    val bestBlock = blockchain.getBestBlock()

    val miningConfig = new MiningConfig {
      override val blockCacheSize: Int = 30
      override val ommersPoolSize: Int = 30
      override val ommerPoolQueryTimeout: FiniteDuration = Timeouts.normalTimeout
      override val headerExtraData: ByteString = ByteString("mined with etc scala")
    }

    lazy val blockTimestampProvider = new FakeBlockTimestampProvider

    lazy val blockGenerator = new BlockGenerator(
      blockchain,
      blockchainConfig,
      miningConfig,
      ledger,
      validators,
      blockTimestampProvider,
      (blockHeader: BlockHeader, Address) => Some(SignedBlockHeader(blockHeader, FakeSignature))
    )
  }
}

class FakeBlockTimestampProvider extends BlockTimestampProvider {
  private var timestamp = Instant.now.getEpochSecond

  def advance(seconds: Long): Unit = timestamp += seconds

  override def getEpochSecond: Long = timestamp
}
