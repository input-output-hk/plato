package io.iohk.ethereum

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import io.iohk.ethereum.mining.ProofOfStakeMiner.StartMining
import io.iohk.ethereum.timing.Beacon
import io.iohk.ethereum.timing.Beacon.Start
import io.iohk.ethereum.utils.Logger
import scala.concurrent.duration._


class MinerMock extends Actor with ActorLogging
{
  override def receive: Receive = {
    case StartMining(slotNumber) => log.debug(s"Start mining at slot $slotNumber")
  }
}


object Test extends Logger {
  def main(args: Array[String]): Unit = {
    log.debug("Starting")
    val system = ActorSystem("Test")
    val slotDuration: FiniteDuration = 2.seconds

    val miner: ActorRef = system.actorOf(Props[MinerMock], "Miner")
    val beacon: ActorRef = system.actorOf(Props(classOf[Beacon],
      miner, slotDuration, System.currentTimeMillis, None), "Beacon")

    beacon ! Start
  }
}
