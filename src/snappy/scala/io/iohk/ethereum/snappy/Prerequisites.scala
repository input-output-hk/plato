package io.iohk.ethereum.snappy

import io.iohk.ethereum.blockchain.data.GenesisDataLoader
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.domain.{Address, BlockchainImpl}
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.nodebuilder._
import io.iohk.ethereum.snappy.Config.{DualDB, SingleDB}
import io.iohk.ethereum.snappy.Prerequisites._
import io.iohk.ethereum.utils.OuroborosConfig
import io.iohk.ethereum.vm.VM
import scala.concurrent.duration._


object Prerequisites {
  trait NoPruning extends PruningModeComponent {
    val pruningMode = ArchivePruning
  }

  trait Storages extends SharedLevelDBDataSources with NoPruning with Storages.DefaultStorages
}

class Prerequisites(config: Config) {

  private def levelDb(dbPath: String): LevelDBDataSource =
    LevelDBDataSource(
      new LevelDbConfig {
        val verifyChecksums: Boolean = true
        val paranoidChecks: Boolean = true
        val createIfMissing: Boolean = true
        val path: String = dbPath
      }
    )

  private val sourceStorages: Storages = new Storages {
    override lazy val dataSource = levelDb(config.sourceDbPath)
  }

  private val targetStorages: Option[Storages] = config.mode match {
    case DualDB =>
      Some(new Storages {
        override lazy val dataSource = levelDb(config.targetDbPath)
      })

    case SingleDB => None
  }

  val sourceBlockchain = BlockchainImpl(sourceStorages.storages)
  val targetBlockchain = targetStorages.map(ts => BlockchainImpl(ts.storages))

  private val components = new ValidatorsBuilder with ClockBuilder with NTPServiceBuilder
    with BlockchainConfigBuilder with SyncConfigBuilder
    with ElectionManagerBuilder with SlotTimeConverterBuilder
    with OuroborosConfigBuilder with BlockchainBuilder with StorageBuilder {
    override lazy val blockchain = sourceBlockchain
    override lazy val storagesInstance = sourceStorages
  }


  val ledger: Ledger = targetBlockchain match {
    case Some(tb) =>
      new LedgerImpl(VM, tb, components.blockchainConfig, components.syncConfig, components.validators)

    case None =>
      new LedgerImpl(VM, sourceBlockchain, components.blockchainConfig, components.syncConfig, components.validators)
  }

  val ouroborosConfig = new OuroborosConfig {
    override val consensusContractFilepath: String = "src/test/resources/CertificateAuthorityManager"

    // unused
    override val consensusContractAddress: Address = Address(0)
    override val slotDuration: FiniteDuration = 0.millis
    override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = Map.empty
  }

  targetBlockchain.foreach { blockchain =>
    val genesisLoader = new GenesisDataLoader(ouroborosConfig, blockchain, components.blockchainConfig, VM)
    genesisLoader.loadGenesisData()
  }
}
