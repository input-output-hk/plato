package io.iohk.ethereum.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActor, TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MiningConfig}
import io.iohk.ethereum.validators.{BlockHeaderValid, BlockHeaderValidatorImpl}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers, Tag}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._

object ProofOfStakeMinerSpec {
  val ProofOfStakeMinerSpecTag = Tag("ProofOfStakeMinerSpec")
}

// scalastyle:off magic.number
class ProofOfStakeMinerSpec extends FlatSpec with Matchers {

  import ProofOfStakeMinerSpec._

  "Miner" should "mine valid block" taggedAs(ProofOfStakeMinerSpecTag) in new TestSetup {
    val parent = origin
    val bfm = blockForMining(parent.header)

    (blockchain.getBestBlock _).expects().returns(parent).anyNumberOfTimes()
    (blockGenerator.generateBlockForMining _).expects(
      parent, Nil, Nil, stakeholder, currentSlotNumber
    ).returning(Right(PendingBlock(bfm, Nil))).anyNumberOfTimes()
    val knownStakeholders = List(stakeholder)
    val keyStore = Mocks.MockKeyStore(Right(knownStakeholders))
    def isValid(stakeholders: Seq[Address])(stakeHolder: Address): Boolean = {
      stakeholders.contains(stakeHolder)
    }
    val electionManager = Mocks.MockElectionManager(isValid(knownStakeholders))

    val miner = TestActorRef(ProofOfStakeMiner.props(
      blockchain,
      blockGenerator,
      ommersPool.ref,
      pendingTransactionsManager.ref,
      syncController.ref,
      miningConfig,
      keyStore,
      electionManager
    ))

    miner ! ProofOfStakeMiner.StartMining(currentSlotNumber)
    val block = waitForMinedBlock()
    block.body.transactionList shouldBe Seq(txToMine)
    blockHeaderValidator.validate(block.header, parent.header) shouldBe Right(BlockHeaderValid)
  }

  "Miner" should "not mine a block if there isn't a stakeholder leader selected" taggedAs(ProofOfStakeMinerSpecTag) in new TestSetup {
    val parent = origin
    val bfm = blockForMining(parent.header)

    (blockchain.getBestBlock _).expects().returns(parent).anyNumberOfTimes()
    (blockGenerator.generateBlockForMining _).expects(
      parent, Nil, Nil, stakeholder, currentSlotNumber
    ).returning(Right(PendingBlock(bfm, Nil))).anyNumberOfTimes()
    val knownStakeholders = List(stakeholder, anotherStakeholder)
    val keyStore = Mocks.MockKeyStore(Right(knownStakeholders))
    def isValid(stakeholders: Seq[Address])(stakeHolder: Address): Boolean = {
      stakeholders.contains(stakeHolder)
    }
    val electionManager = Mocks.MockElectionManager(isValid(List.empty))

    val miner = TestActorRef(ProofOfStakeMiner.props(
      blockchain,
      blockGenerator,
      ommersPool.ref,
      pendingTransactionsManager.ref,
      syncController.ref,
      miningConfig,
      keyStore,
      electionManager
    ))

    miner ! ProofOfStakeMiner.StartMining(currentSlotNumber)
    syncController.expectNoMsg()
  }

  trait TestSetup extends MockFactory {

    val miningConfig = MiningConfig(Config.config)

    val previousSlotNumber = BigInt(1)
    val currentSlotNumber = BigInt(2)

    val origin = Block(
      BlockHeader(
        parentHash = ByteString(Hex.decode("0000000000000000000000000000000000000000000000000000000000000000")),
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = ByteString(Hex.decode("0000000000000000000000000000000000000000")),
        stateRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        transactionsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        receiptsRoot = ByteString(Hex.decode("56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421")),
        logsBloom = ByteString(Hex.decode("00" * 256)),
        difficulty = UInt256(Hex.decode("0400")).toBigInt,
        number = previousSlotNumber,
        gasLimit = UInt256(Hex.decode("ff1388")).toBigInt,
        gasUsed = 0,
        unixTimestamp = 0,
        extraData = miningConfig.headerExtraData,
        mixHash = ByteString.empty,
        nonce = ByteString.empty,
        slotNumber = previousSlotNumber
      ),
      BlockBody(Seq(), Seq())
    )

    val blockchain = mock[BlockchainImpl]
    val blockGenerator: BlockGenerator = mock[BlockGenerator]
    val blockchainConfig = BlockchainConfig(Config.config)
    val difficultyCalc = new DifficultyCalculator(blockchainConfig)
    val blockForMiningTimestamp = System.currentTimeMillis()
    private def calculateGasLimit(parentGas: BigInt): BigInt = {
      val GasLimitBoundDivisor: Int = 1024
      val gasLimitDifference = parentGas / GasLimitBoundDivisor
      parentGas + gasLimitDifference - 1
    }

    val txToMine = SignedTransaction(
      tx = Transaction(
        nonce = BigInt("438553"),
        gasPrice = BigInt("20000000000"),
        gasLimit = BigInt("50000"),
        receivingAddress = Address(ByteString(Hex.decode("3435be928d783b7c48a2c3109cba0d97d680747a"))),
        value = BigInt("108516826677274384"),
        payload = ByteString.empty
      ),
      pointSign = 0x9d.toByte,
      signatureRandom = ByteString(Hex.decode("beb8226bdb90216ca29967871a6663b56bdd7b86cf3788796b52fd1ea3606698")),
      signature = ByteString(Hex.decode("2446994156bc1780cb5806e730b171b38307d5de5b9b0d9ad1f9de82e00316b5")),
      chainId = 0x3d.toByte
    ).get

    val stakeholder = Address("0x01")
    val anotherStakeholder = Address("0x10")

    def blockForMining(parent: BlockHeader): Block = {
      Block(BlockHeader(
        parentHash = parent.hash,
        ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
        beneficiary = stakeholder.bytes,
        stateRoot = parent.stateRoot,
        transactionsRoot = parent.transactionsRoot,
        receiptsRoot = parent.receiptsRoot,
        logsBloom = parent.logsBloom,
        difficulty = difficultyCalc.calculateDifficulty(1, blockForMiningTimestamp, parent),
        number = currentSlotNumber,
        gasLimit = calculateGasLimit(parent.gasLimit),
        gasUsed = BigInt(0),
        unixTimestamp = blockForMiningTimestamp,
        extraData = miningConfig.headerExtraData,
        mixHash = ByteString.empty,
        nonce = ByteString.empty,
        slotNumber = currentSlotNumber
      ), BlockBody(Seq(txToMine), Nil))
    }

    implicit val system = ActorSystem("ProofOfStakeMinerSpec_System")

    val blockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
    val ommersPool = TestProbe()
    val pendingTransactionsManager = TestProbe()
    val syncController = TestProbe()

    def waitForMinedBlock(): Block = {
      syncController.expectMsgPF[Block](10.minutes) {
        case m: RegularSync.MinedBlock => m.block
      }
    }
    ommersPool.setAutoPilot(new TestActor.AutoPilot {
      def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
        sender ! OmmersPool.Ommers(Nil)
        TestActor.KeepRunning
      }
    })
    pendingTransactionsManager.setAutoPilot(new TestActor.AutoPilot {
      def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
        sender ! PendingTransactionsManager.PendingTransactionsResponse(Nil)
        TestActor.KeepRunning
      }
    })
  }
}
