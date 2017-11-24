package io.iohk.ethereum.ommers

import akka.actor.{Actor, Props}
import io.iohk.ethereum.domain.{Blockchain, SignedBlockHeader}
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, GetOmmers, RemoveOmmers}
import io.iohk.ethereum.utils.MiningConfig

class OmmersPool(blockchain: Blockchain, miningConfig: MiningConfig) extends Actor {

  var ommersPool: Seq[SignedBlockHeader] = Nil

  val ommerGenerationLimit: Int = 6 //Stated on section 11.1, eq. (143) of the YP
  val ommerSizeLimit: Int = 2

  override def receive: Receive = {
    case AddOmmers(ommers) =>
      ommersPool = (ommers ++ ommersPool).take(miningConfig.ommersPoolSize).distinct

    case RemoveOmmers(ommers) =>
      val toDelete = ommers.map(_.hash).toSet
      ommersPool = ommersPool.filter(b => !toDelete.contains(b.hash))

    case GetOmmers(blockNumber) =>
      val ommers = ommersPool.filter { b =>
        val generationDifference = blockNumber - b.header.number
        generationDifference > 0 && generationDifference <= ommerGenerationLimit
      }.filter { b =>
        blockchain.getSignedBlockHeaderByHash(b.header.parentHash).isDefined
      }.take(ommerSizeLimit)
      sender() ! OmmersPool.Ommers(ommers)
  }
}

object OmmersPool {
  def props(blockchain: Blockchain, miningConfig: MiningConfig): Props = Props(new OmmersPool(blockchain, miningConfig))

  case class AddOmmers(ommers: List[SignedBlockHeader])

  object AddOmmers {
    def apply(b: SignedBlockHeader*): AddOmmers = AddOmmers(b.toList)
  }

  case class RemoveOmmers(ommers: List[SignedBlockHeader])

  object RemoveOmmers {
    def apply(b: SignedBlockHeader*): RemoveOmmers = RemoveOmmers(b.toList)
  }

  case class GetOmmers(blockNumber: BigInt)

  case class Ommers(headers: Seq[SignedBlockHeader])
}
