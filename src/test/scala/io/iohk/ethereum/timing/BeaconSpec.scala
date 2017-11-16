package io.iohk.ethereum.timing

import akka.actor.{ActorSystem, Props, Scheduler}
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.mining.ProofOfStakeMiner.StartMining
import io.iohk.ethereum.timing.Beacon.Start
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._


class BeaconSpec extends FlatSpec with Matchers {

  "The Beacon" should "immediately send the slot number 1 upon start-up" in new TestSetup() {
    beacon ! Start
    minerMock.expectMsg(StartMining(1))
  }

  // TODO: Add the other tests.

}

class TestSetup extends MockFactory {

  implicit val system = ActorSystem("BeaconSpec_System")
  val minerMock = TestProbe()
  val slotDuration: FiniteDuration = 2.seconds
  val schedulerMock = stub[Scheduler]
  val time = new VirtualTime
  val beacon = TestActorRef(Props(new Beacon(
    minerMock.ref, slotDuration, System.currentTimeMillis, Some(schedulerMock))))
}
