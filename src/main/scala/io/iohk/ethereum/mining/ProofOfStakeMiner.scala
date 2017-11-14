package io.iohk.ethereum.mining

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.PendingTransactionsResponse
import io.iohk.ethereum.utils.MiningConfig

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ProofOfStakeMiner(
             blockchain: Blockchain,
             blockGenerator: BlockGenerator,
             ommersPool: ActorRef,
             pendingTransactionsManager: ActorRef,
             syncController: ActorRef,
             miningConfig: MiningConfig)
  extends Actor with ActorLogging {

  import ProofOfStakeMiner._
  import akka.pattern.ask

  override def receive: Receive = {
    case StartMining(slotNumber) => processMining(slotNumber)
  }

  def processMining(slotNumber: BigInt): Unit = {
    val parentBlock = blockchain.getBestBlock()
    // TODO: For each stakeholder check if is leader, if it is the case generate a new block!
    getBlockForMining(parentBlock, slotNumber) onComplete {
      case Success(PendingBlock(block, _)) =>
        syncController ! RegularSync.MinedBlock(block)
      case Failure(ex) =>
        log.error(ex, "Unable to get block for mining")
    }
  }

  private def getBlockForMining(parentBlock: Block, slotNumber: BigInt): Future[PendingBlock] = {
    getOmmersFromPool(parentBlock.header.number + 1).zip(getTransactionsFromPool).flatMap { case (ommers, pendingTxs) =>
      blockGenerator.generateBlockForMining(parentBlock, pendingTxs.pendingTransactions.map(_.stx), ommers.headers, miningConfig.coinbase, slotNumber) match {
        case Right(pb) => Future.successful(pb)
        case Left(err) => Future.failed(new RuntimeException(s"Error while generating block for mining: $err"))
      }
    }
  }

  private def getOmmersFromPool(blockNumber: BigInt) = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (ommersPool ? OmmersPool.GetOmmers(blockNumber)).mapTo[OmmersPool.Ommers]
      .recover { case ex =>
        log.error(ex, "Failed to get ommers, mining block with empty ommers list")
        OmmersPool.Ommers(Nil)
      }
  }

  private def getTransactionsFromPool = {
    implicit val timeout = Timeout(miningConfig.ommerPoolQueryTimeout)

    (pendingTransactionsManager ? PendingTransactionsManager.GetPendingTransactions).mapTo[PendingTransactionsResponse]
      .recover { case ex =>
        log.error(ex, "Failed to get transactions, mining block with empty transactions list")
        PendingTransactionsResponse(Nil)
      }
  }
}

object ProofOfStakeMiner {
  def props(blockchain: Blockchain,
            blockGenerator: BlockGenerator,
            ommersPool: ActorRef,
            pendingTransactionsManager: ActorRef,
            syncController: ActorRef,
            miningConfig: MiningConfig): Props =
    Props(new Miner(blockchain, blockGenerator, ommersPool, pendingTransactionsManager, syncController, miningConfig))

  case class StartMining(slotNumber: BigInt)
}
