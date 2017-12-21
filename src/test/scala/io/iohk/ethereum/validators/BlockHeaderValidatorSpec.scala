package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.blockchain.EphemBlockchainTestSetup
import io.iohk.ethereum.{Fixtures, ObjectGenerators}
import io.iohk.ethereum.domain.{UInt256, _}
import io.iohk.ethereum.pos.ElectionManagerImpl
import io.iohk.ethereum.timing.Clock
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig}
import io.iohk.ethereum.validators.BlockHeaderError._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.validators.BlockHeaderValidatorImpl._
import org.scalamock.scalatest.MockFactory
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.crypto.{generateKeyPair, kec256}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

import scala.concurrent.duration._

class BlockHeaderValidatorSpec extends FlatSpec with Matchers with PropertyChecks with ObjectGenerators with MockFactory with SecureRandomBuilder {

  "BlockHeaderValidator" should "validate correctly formed SignedBlockHeaders" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    blockHeaderValidator.validate(validSignedBlockHeader, validSignedBlockHeaderParent.header) match {
      case Right(_) => succeed
      case _ => fail
    }
  }

  it should "return a failure if created based on invalid extra data" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    forAll(randomSizeByteStringGen(
      MaxExtraDataSize + 1,
      MaxExtraDataSize + ExtraDataSizeLimit)
    ) { wrongExtraData =>
      val invalidBlockHeader = validSignedBlockHeader.header.copy(extraData = wrongExtraData)
      val invalidSignedBlockHeader = SignedBlockHeader.sign(invalidBlockHeader, validBlockBeneficiaryKeyPair)
      assert(blockHeaderValidator.validate(invalidSignedBlockHeader, validSignedBlockHeaderParent.header) == Left(HeaderExtraDataError))
    }
  }

  it should "return a failure if created based on invalid timestamp" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    forAll(longGen) { timestamp =>
      val blockHeaderWithInvalidTimestamp = validSignedBlockHeader.header.copy(unixTimestamp = timestamp)
      val signedBlockHeaderWithInvalidTimestamp = SignedBlockHeader.sign(blockHeaderWithInvalidTimestamp, validBlockBeneficiaryKeyPair)
      val validateResult = blockHeaderValidator.validate(signedBlockHeaderWithInvalidTimestamp, validSignedBlockHeaderParent.header)
      timestamp match {
        case t if t <= validSignedBlockHeaderParent.header.unixTimestamp => assert(validateResult == Left(HeaderTimestampError))
        case _ => assert(validateResult == Right(BlockHeaderValid))
      }
    }
  }

  it should "return a failure if created based on invalid difficulty" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    forAll(bigIntGen) { difficulty =>
      val blockHeader = validSignedBlockHeader.header.copy(difficulty = difficulty)
      val signedBlockHeader = SignedBlockHeader.sign(blockHeader, validBlockBeneficiaryKeyPair)
      val validateResult = blockHeaderValidator.validate(signedBlockHeader, validSignedBlockHeaderParent.header)
      if (difficulty != validSignedBlockHeader.header.difficulty) assert(validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created based on invalid gas used" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    forAll(bigIntGen) { gasUsed =>
      val blockHeader = validSignedBlockHeader.header.copy(gasUsed = gasUsed)
      val signedBlockHeader = SignedBlockHeader.sign(blockHeader, validBlockBeneficiaryKeyPair)
      val validateResult = blockHeaderValidator.validate(signedBlockHeader, validSignedBlockHeaderParent.header)
      if (gasUsed > validSignedBlockHeader.header.gasLimit) assert(validateResult == Left(HeaderGasUsedError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created based on invalid gas limit" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    val LowerGasLimit = MinGasLimit.max(
      validSignedBlockHeaderParent.header.gasLimit - validSignedBlockHeaderParent.header.gasLimit / GasLimitBoundDivisor + 1)
    val UpperGasLimit = validSignedBlockHeaderParent.header.gasLimit + validSignedBlockHeaderParent.header.gasLimit / GasLimitBoundDivisor - 1

    forAll(bigIntGen) { gasLimit =>
      val blockHeader = validSignedBlockHeader.header.copy(gasLimit = gasLimit)
      val signedBlockHeader = SignedBlockHeader.sign(blockHeader, validBlockBeneficiaryKeyPair)
      val validateResult = blockHeaderValidator.validate(signedBlockHeader, validSignedBlockHeaderParent.header)
      if (gasLimit < LowerGasLimit || gasLimit > UpperGasLimit)
        assert(validateResult == Left(HeaderGasLimitError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "return a failure if created with gas limit above threshold and block number >= eip106 block number" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    val validParent = validSignedBlockHeaderParent.header.copy(gasLimit = Long.MaxValue)
    val invalidBlockHeader = validSignedBlockHeader.header.copy(gasLimit = BigInt(Long.MaxValue) + 1)
    val invalidSignedBlockHeader = SignedBlockHeader.sign(invalidBlockHeader, validBlockBeneficiaryKeyPair)
    blockHeaderValidator.validate(invalidSignedBlockHeader, validParent) shouldBe Left(HeaderGasLimitError)
  }

  it should "return a failure if created based on invalid number" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    forAll(longGen) { number =>
      val blockHeader = validSignedBlockHeader.header.copy(number = number)
      val signedBlockHeader = SignedBlockHeader.sign(blockHeader, validBlockBeneficiaryKeyPair)
      val validateResult = blockHeaderValidator.validate(signedBlockHeader, validSignedBlockHeaderParent.header)
      if (number != validSignedBlockHeaderParent.header.number + 1)
        assert(validateResult == Left(HeaderNumberError) || validateResult == Left(HeaderDifficultyError))
      else assert(validateResult == Right(BlockHeaderValid))
    }
  }

  it should "validate correctly a block whose parent is in storage" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    blockchain.save(validSignedBlockHeaderParent)
    blockHeaderValidator.validate(validSignedBlockHeader, blockchain) match {
      case Right(_) => succeed
      case _ => fail
    }
  }

  it should "return a failure if the parent's header is not in storage" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    blockHeaderValidator.validate(validSignedBlockHeader, blockchain) match {
      case Left(HeaderParentNotFoundError) => succeed
      case _ => fail
    }
  }

  it should "return a failure if the slot number is lower than it's parent's" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    val invalidBlockHeader = validSignedBlockHeader.header.copy(slotNumber = validSignedBlockHeaderParent.header.slotNumber - 1)
    val invalidSignedBlockHeader = SignedBlockHeader.sign(invalidBlockHeader, validBlockBeneficiaryKeyPair)
    blockHeaderValidator.validate(invalidSignedBlockHeader, validSignedBlockHeaderParent.header) shouldBe Left(HeaderSlotNumberError)
  }

  it should "return a failure if the block is from a future slot" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime - 1.millis).anyNumberOfTimes()

    blockHeaderValidator.validate(validSignedBlockHeader, validSignedBlockHeaderParent.header) shouldBe Left(HeaderSlotNumberError)
  }

  it should "return a failure if the block beneficiary wasn't the slot leader" in new TestSetup {
    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(false).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    val invalidBlockHeader = validSignedBlockHeader.header.copy(slotNumber = validSignedBlockHeaderParent.header.slotNumber - 1)
    val invalidSignedBlockHeader = SignedBlockHeader.sign(invalidBlockHeader, validBlockBeneficiaryKeyPair)
    blockHeaderValidator.validate(invalidSignedBlockHeader, validSignedBlockHeaderParent.header) shouldBe Left(HeaderBeneficiaryError)
  }

  it should "return a failure if the block is signed with a signature different to the block beneficiary signature" in new TestSetup {

    val anotherBeneficiaryKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
    val signedBlockHeaderWithInvalidSignature = SignedBlockHeader.sign(validBlockHeader, anotherBeneficiaryKeyPair)

    (electionManagerMock.verifyIsLeader _).expects(*, *).returning(true).anyNumberOfTimes()
    (slotCalculatorMock.getSlotStartingMillis _).expects(*).returning(CurrentTime.toMillis - 1).anyNumberOfTimes()
    (clockMock.now _).expects().returning(CurrentTime).anyNumberOfTimes()

    blockHeaderValidator.validate(signedBlockHeaderWithInvalidSignature, validSignedBlockHeaderParent.header) shouldBe Left(SignedHeaderSignatureError)
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    val ExtraDataSizeLimit = 20

    //BlockHeader member's lengths obtained from Yellow paper
    val NonceLength = 8 //64bit
    val MixHashLength = 32 //256bit

    val blockchainConfig = createBlockchainConfig()
    val electionManagerMock = mock[ElectionManagerImpl]
    val slotCalculatorMock = mock[SlotTimeConverter]

    val clockMock = mock[Clock]
    val blockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig, electionManagerMock, slotCalculatorMock, clockMock)
    val difficultyCalculator = new DifficultyCalculator(blockchainConfig)

    val CurrentTime = 2.millis
  }

  val pausedDifficultyBombBlockBeneficiaryKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val pausedDifficultyBombBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("77af90df2b60071da7f11060747b6590a3bc2f357da4addccb5eef7cb8c2b723")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(pausedDifficultyBombBlockBeneficiaryKeyPair),
    stateRoot = ByteString(Hex.decode("32deebbf585e9b0d0153b96d62283e903c10fac41fc4181438e29732c490ac6e")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 1,
    number = 3582022,
    gasLimit = 4700036,
    gasUsed = 0,
    unixTimestamp = 1492735637,
    extraData = ByteString(Hex.decode("d58301050b8650617269747986312e31352e31826c69")),
    mixHash = ByteString(Hex.decode("7d2db22c3dfaccb1b6927f5675ec24a41991ee4bcffdc564f940a45c1fce8acb")),
    nonce = ByteString(Hex.decode("81d6a5e8029f9446")),
    slotNumber = 3582022
  )
  val pausedDifficultyBombBlockSignedBlockHProDaoBlock1920009KeyPaireader = SignedBlockHeader.sign(pausedDifficultyBombBlockHeader, pausedDifficultyBombBlockBeneficiaryKeyPair)

  val pausedDifficultyBombBlockParentKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val pausedDifficultyBombBlockParentBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("e6e90c1ba10df710365a2ae9f899bd787416d98f19874f4cb1a62f09c3b8277d")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(pausedDifficultyBombBlockParentKeyPair),
    stateRoot = ByteString(Hex.decode("0920dc025715c278dc297aa7b2d1bf5a60666d92be22d338135d13571539fad7")),
    transactionsRoot = ByteString(Hex.decode("6616c23aeb486dd47aca667814ffed831553c7322440913b95847235a4c3bb97")),
    receiptsRoot = ByteString(Hex.decode("5fa90473cd08a08fc766329651d81bb6e4ef2bb330cf90c3025927a3bafe0c57")),
    logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000080000000000000000020000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000008000000000000000")),
    difficulty = 1,
    number = 3582021,
    gasLimit = 4699925,
    gasUsed = 1005896,
    unixTimestamp = 1492735634,
    extraData = ByteString(Hex.decode("d58301050c8650617269747986312e31362e30826c69")),
    mixHash = ByteString(Hex.decode("d10215664192800200eab9ca7b90f9ceb8d8428200c2b4e6aebe2191c2a52c0e")),
    nonce = ByteString(Hex.decode("83e2d9b401cdfa77")),
    slotNumber = 3582021
  )
  val pausedDifficultyBombBlockParent = SignedBlockHeader.sign(pausedDifficultyBombBlockParentBlockHeader, pausedDifficultyBombBlockParentKeyPair)

  val validBlockParentBeneficiaryKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val validBlockParentBlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("677a5fb51d52321b03552e3c667f602cc489d15fc1d7824445aee6d94a9db2e7")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(validBlockParentBeneficiaryKeyPair),
    stateRoot = ByteString(Hex.decode("cddeeb071e2f69ad765406fb7c96c0cd42ddfc6ec54535822b564906f9e38e44")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 1,
    number = 19,
    gasLimit = 131749155,
    gasUsed = 0,
    unixTimestamp = 1486752440,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("7f9ac1ddeafff0f926ed9887b8cf7d50c3f919d902e618b957022c46c8b404a6")),
    nonce = ByteString(Hex.decode("3fc7bc671f7cee70")),
    slotNumber = 19
  )
  val validSignedBlockHeaderParent = SignedBlockHeader.sign(validBlockParentBlockHeader, validBlockParentBeneficiaryKeyPair)

  val validBlockBeneficiaryKeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val validBlockHeader = BlockHeader(
    parentHash = validSignedBlockHeaderParent.hash,
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(validBlockBeneficiaryKeyPair),
    stateRoot = ByteString(Hex.decode("634a2b20c9e02afdda7157afe384306c5acc4fb9c09b45dc0203c0fbb2fed0e6")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 1,
    number = 20,
    gasLimit = 131620495,
    gasUsed = 0,
    unixTimestamp = 1486752441,
    extraData = ByteString(Hex.decode("d783010507846765746887676f312e372e33856c696e7578")),
    mixHash = ByteString(Hex.decode("6bc729364c9b682cfa923ba9480367ebdfa2a9bca2a652fe975e8d5958f696dd")),
    nonce = ByteString(Hex.decode("797a8f3a494f937b")),
    slotNumber = 20
  )
  val validSignedBlockHeader = SignedBlockHeader.sign(validBlockHeader, validBlockBeneficiaryKeyPair)

  def createBlockchainConfig(supportsDaoFork: Boolean = false): BlockchainConfig =
    new BlockchainConfig {

      import Fixtures.Blocks._

      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000

      override val daoForkConfig: Option[DaoForkConfig] = Some(new DaoForkConfig {
        override val blockExtraData: Option[ByteString] = if (supportsDaoFork) Some(ProDaoForkBlock.signedHeader.header.extraData) else None
        override val range: Int = 10
        override val drainList: Seq[Address] = Nil
        override val forkBlockHash: ByteString = if (supportsDaoFork) ProDaoForkBlock.signedHeader.hash else DaoForkBlock.signedHeader.hash
        override val forkBlockNumber: BigInt = DaoForkBlock.signedHeader.header.number
        override val refundContract: Option[Address] = None
      })

      // unused
      override val maxCodeSize: Option[BigInt] = None
      override val eip155BlockNumber: BigInt = Long.MaxValue
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip161BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val eip106BlockNumber: BigInt = 0
      override val chainId: Byte = 0x3d.toByte
      override val monetaryPolicyConfig: MonetaryPolicyConfig = null
      override val customGenesisFileOpt: Option[String] = None
      override val accountStartNonce: UInt256 = UInt256.Zero
      val gasTieBreaker: Boolean = false
    }

  val ProDaoBlock1920008KeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val ProDaoBlock1920008BlockHeader = BlockHeader(
    parentHash = ByteString(Hex.decode("05c45c9671ee31736b9f37ee98faa72c89e314059ecff3257206e6ab498eb9d1")),
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(ProDaoBlock1920008KeyPair),
    stateRoot = ByteString(Hex.decode("fa8d3b3cbd37caba2faf09d5e472ae6c47a58d846751bc72306166a71d0fa4fa")),
    transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
    logsBloom = ByteString(Hex.decode("00" * 256)),
    difficulty = 1,
    number = 1920008,
    gasLimit = 4707788,
    gasUsed = 0,
    unixTimestamp = 1469021025,
    extraData = ByteString(Hex.decode("64616f2d686172642d666f726b")),
    mixHash = ByteString(Hex.decode("e73421390c1b084a9806754b238715ec333cdccc8d09b90cb6e38a9d1e247d6f")),
    nonce = ByteString(Hex.decode("c207c8381305bef2")),
    slotNumber = 1920008
  )
  val ProDaoBlock1920008Header = SignedBlockHeader.sign(ProDaoBlock1920008BlockHeader, ProDaoBlock1920008KeyPair)

  val ProDaoBlock1920009KeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val ProDaoBlock1920009BlockHeader = BlockHeader(
    parentHash = ProDaoBlock1920008Header.hash,
    ommersHash = ByteString(Hex.decode("808d06176049aecfd504197dde49f46c3dd75f1af055e417d100228162eefdd8")),
    beneficiary = getAddressBytesFromKeyPair(ProDaoBlock1920009KeyPair),
    stateRoot = ByteString(Hex.decode("49eb333152713b78d920440ef065ed7f681611e0c2e6933d657d6f4a7f1936ee")),
    transactionsRoot = ByteString(Hex.decode("a8060f1391fd4cbde4b03d83b32a1bda445578cd6ec6b7982db20c499ed3682b")),
    receiptsRoot = ByteString(Hex.decode("ab66b1986e713eaf5621059e79f04ba9c528187c1b9da969f46442c3f915c120")),
    logsBloom = ByteString(Hex.decode("00000000000000020000000000020000000000000008000000000000000000000000000000000000000000000000400000000000000000000000000000202010000000000000000000000008000000000000000000000000400000000000000000000800000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000001001000020000000000000000000000000000000000000000000000000000000000000000000002000000000000000000000000004000000000000000000000000000000010000000000000000000000000000000100000000000000000000000000000")),
    difficulty = 1,
    number = 1920009,
    gasLimit = 4712384,
    gasUsed = 109952,
    unixTimestamp = 1469021040,
    extraData = ByteString(Hex.decode("64616f2d686172642d666f726b")),
    mixHash = ByteString(Hex.decode("5bde79f4dc5be28af2d956e748a0d6ebc1f8eb5c1397e76729269e730611cb99")),
    nonce = ByteString(Hex.decode("2b4b464c0a4da82a")),
    slotNumber = 1920009
  )
  val ProDaoBlock1920009Header = SignedBlockHeader.sign(ProDaoBlock1920009BlockHeader, ProDaoBlock1920009KeyPair)

  val ProDaoBlock1920010KeyPair: AsymmetricCipherKeyPair = generateKeyPair(secureRandom)
  val ProDaoBlock1920010BlockHeader = BlockHeader(
    parentHash = ProDaoBlock1920009Header.hash,
    ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
    beneficiary = getAddressBytesFromKeyPair(ProDaoBlock1920010KeyPair),
    stateRoot = ByteString(Hex.decode("6ee63abee7416d3a671bcbefa01aa5d4ea427e246d548e15c5f3d9a108e738fd")),
    transactionsRoot = ByteString(Hex.decode("0c6d4a643ed081f92e384a5853f14d7f5ff5d68b65d0c90b46159584a80effe0")),
    receiptsRoot = ByteString(Hex.decode("a7d1ddb80060d4b77c07007e9a9f0b83413bd2c5de71501683ba4764982eef4b")),
    logsBloom = ByteString(Hex.decode("00000000000000020000000000020000001000000000000000000000000000000008000000000000000000000000400000000000000000000000000000202000000000000800000000000008000000000000000000000000400000000008000000000000000000000000000000000000000000000000000000000010000000000000000000000000000221000000000000000000080400000000000000011000020000000200001000000000000000000000000000000000400000000000000000000002000000000100000000000000000000000040000000000000000000000010000000000000000000000000000000000000000000000000000000000000")),
    difficulty = 1,
    number = 1920010,
    gasLimit = 4712388,
    gasUsed = 114754,
    unixTimestamp = 1469021050,
    extraData = ByteString(Hex.decode("657468706f6f6c2e6f7267202855533129")),
    mixHash = ByteString(Hex.decode("8f86617d6422c26a89b8b349b160973ca44f90326e758f1ef669c4046741dd06")),
    nonce = ByteString(Hex.decode("c7de19e00a8c3e32")),
    slotNumber = 1920010
  )
  val ProDaoBlock1920010Header = SignedBlockHeader.sign(ProDaoBlock1920010BlockHeader, ProDaoBlock1920010KeyPair)

  private def getAddressBytesFromKeyPair(keyPair: AsymmetricCipherKeyPair): ByteString =
    Address(kec256(keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail)).bytes
}
