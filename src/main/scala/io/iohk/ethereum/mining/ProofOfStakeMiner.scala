package io.iohk.ethereum.mining

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.RegularSync
import io.iohk.ethereum.domain.{Address, Block, Blockchain}
import io.iohk.ethereum.keystore.KeyStore
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.pos.ElectionManager
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
             miningConfig: MiningConfig,
             keyStore: KeyStore,
             electionManager: ElectionManager)
  extends Actor with ActorLogging {

  import ProofOfStakeMiner._
  import akka.pattern.ask

  override def receive: Receive = {
    case StartMining(slotNumber) => processMining(slotNumber)
  }

  def processMining(slotNumber: BigInt): Unit = {
    keyStore.listAccounts() match {
      case Right(accounts) =>
        val mayBeLeader: Option[Address] = accounts.collectFirst {
          case account if electionManager.verifyIsLeader(account, slotNumber) => account
        }
        mayBeLeader match {
          case Some(leader) =>
            val parentBlock = blockchain.getBestBlock()
            getBlockForMining(parentBlock, slotNumber, leader) onComplete {
              case Success(PendingBlock(block, _)) =>
                syncController ! RegularSync.MinedBlock(block)
              case Failure(ex) =>
                log.error(ex, "Unable to get block for mining")
            }
          case _ =>
            log.info("No leader selected")
        }
      case Left(keyStoreError) =>
        log.error("Error: Unable to access to node accounts", keyStoreError.toString)
    }
  }

  private def getBlockForMining(parentBlock: Block, slotNumber: BigInt, blockLeader: Address): Future[PendingBlock] = {
    getOmmersFromPool(parentBlock.header.number + 1).zip(getTransactionsFromPool).flatMap { case (ommers, pendingTxs) =>
      blockGenerator.generateBlockForMining(parentBlock, pendingTxs.pendingTransactions.map(_.stx), ommers.headers, blockLeader, slotNumber) match {
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
            miningConfig: MiningConfig,
            keyStore: KeyStore,
            electionManager: ElectionManager): Props =
    Props(new ProofOfStakeMiner(
      blockchain,
      blockGenerator,
      ommersPool,
      pendingTransactionsManager,
      syncController,
      miningConfig,
      keyStore,
      electionManager)
    )
  case class StartMining(slotNumber: BigInt)
}
