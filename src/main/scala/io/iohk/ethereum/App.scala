package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSync.{StateMptNodeHash, SyncState}
import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.db.components.StoragesComponent
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.ServerActor
import io.iohk.ethereum.utils.{Config, Logger}
import io.iohk.ethereum.nodebuilder.Node

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object App {

  private def loadFasterFastSync(storagesInstance: StoragesComponent): Unit = {
    import Config.FasterFastSync._

    val baseBlock = BlockHeader(
      parentHash = ByteString("unused"),
      ommersHash = ByteString("unused"),
      beneficiary = ByteString("unused"),
      stateRoot = ByteString("unused"),
      transactionsRoot = ByteString("unused"),
      receiptsRoot = ByteString("unused"),
      logsBloom = ByteString("unused"),
      difficulty = 0,
      number = 0,
      gasLimit = 0,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused")
    )

    val initialState = SyncState(
      targetBlock = baseBlock.copy(number = targetBlockNumber.get),
      mptNodesQueue = Seq(StateMptNodeHash(targetBlockStateRoot.get)),
      bestBlockHeaderNumber = initialBlockNumber.get
    )
    storagesInstance.storages.fastSyncStateStorage.putSyncState(initialState)
    storagesInstance.storages.appStateStorage.putBestBlockNumber(initialBlockNumber.get)
    storagesInstance.storages.totalDifficultyStorage.put(initialBlockHash.get, initialBlockTotalDifficulty.get)
  }

  def main(args: Array[String]): Unit = {

    new Node with Logger {

      def tryAndLogFailure(f: () => Any): Unit = Try(f()) match {
        case Failure(e) => log.warn("Error while shutting down...", e)
        case Success(_) =>
      }

      override def shutdown(): Unit = {
        tryAndLogFailure(() => Await.ready(actorSystem.terminate, shutdownTimeoutDuration))
        tryAndLogFailure(() => storagesInstance.dataSources.closeAll())
      }

      genesisDataLoader.loadGenesisData()

      if(Config.FasterFastSync.useFasterFastSync) {
        if(storagesInstance.storages.appStateStorage.getBestBlockNumber() == 0)
          loadFasterFastSync(storagesInstance)
        else
          log.warn("Client configured for faster fast sync but it cannot start as our DataSource isn't empty")
      }

      server ! ServerActor.StartServer(networkConfig.Server.listenAddress)
      syncController ! SyncController.StartSync

      if(rpcServerConfig.enabled) startJSONRpcServer()
    }

  }
}
