package io.iohk.ethereum.blockchain.data

import akka.util.ByteString
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig, OuroborosConfig}
import io.iohk.ethereum.vm.{ProgramResult, VM}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class GenesisDataLoaderSpec  extends FlatSpec with Matchers {


  it should "load user alloc accounts from genesis file" in new TestSetup {

    val ouroborosConfig = new OuroborosConfig {
      //unused
      override val slotDuration: FiniteDuration = 0.seconds
      override val consensusContractAddress: Address = Address("000000000000000000000000000000000000000a")
      override val consensusContractFilepath: String = "src/test/resources/CertificateAuthorityManager"
      override val initialCA: Address = Address(1)
    }
    val genesisDataLoader = new GenesisDataLoader(ouroborosConfig, blockchain, blockchainConfig, VM)

    genesisDataLoader.loadGenesisData()

    val userAllocAddress1 = Address("d7a681378321f472adffb9fdded2712f677e0ba9")
    val maybeUserAllocAddress1 = blockchain.getAccount(userAllocAddress1, 0)
    maybeUserAllocAddress1.get.balance shouldEqual BigInt("1000000000000000000000000000000000000000000")

    val userAllocAddress2 = Address("18a681378321f472adffb9fdded2712f677e0baf")
    val maybeUserAllocAddress2 = blockchain.getAccount(userAllocAddress2, 0)
    maybeUserAllocAddress2.get.balance shouldEqual BigInt("2000000000000000000000000000000000000000000")
  }

  it should "loading genesis block consensus contract" in new TestSetup {

    val ouroborosConfig = new OuroborosConfig {
      override val consensusContractAddress: Address = Address("000000000000000000000000000000000000000a")
      override val consensusContractFilepath: String = "src/test/resources/CertificateAuthorityManager"
      override val initialCA: Address = Address(1)
      //unused
      override val slotDuration: FiniteDuration = 0.seconds
    }
    val contractCodeHash = ByteString("dummyContractData")
    val contractStateKey = UInt256.One
    val contractStateValue = UInt256.One
    val VM = new MockVM(c => ProgramResult(
      returnData = contractCodeHash,
      gasRemaining = 0,
      world = c.world.saveStorage(
        ouroborosConfig.consensusContractAddress,
        c.world.getStorage(ouroborosConfig.consensusContractAddress).store(contractStateKey, contractStateValue)
      ),
      addressesToDelete = Set.empty,
      logs = Seq.empty,
      internalTxs = Nil,
      gasRefund = 0,
      error = None
    ))
    val genesisDataLoader = new GenesisDataLoader(ouroborosConfig, blockchain, blockchainConfig, VM)

    genesisDataLoader.loadGenesisData()

    val maybeContractAccount = blockchain.getAccount(ouroborosConfig.consensusContractAddress, 0)
    maybeContractAccount.get should not be None
    blockchain.getEvmCodeByHash(maybeContractAccount.get.codeHash).get shouldEqual contractCodeHash
    blockchain.getAccountStorageAt(maybeContractAccount.get.storageRoot, contractStateKey) shouldEqual contractStateValue.bytes
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val blockchainConfig = new BlockchainConfig {
      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
      override val accountStartNonce: UInt256 = UInt256.Zero
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 0
      override val eip150BlockNumber: BigInt = 0
      override val eip160BlockNumber: BigInt = 0
      override val eip161BlockNumber: BigInt = 0
      override val maxCodeSize: Option[BigInt] = None

      //unused
      override val eip155BlockNumber: BigInt = 0
      override val eip106BlockNumber: BigInt = 0
      override val difficultyBombPauseBlockNumber: BigInt = 0
      override val difficultyBombContinueBlockNumber: BigInt = 0
      override val chainId: Byte = 0.toByte
      override val monetaryPolicyConfig: MonetaryPolicyConfig = null
      override val daoForkConfig: Option[DaoForkConfig] = None
      val gasTieBreaker: Boolean = false
    }
  }
}
