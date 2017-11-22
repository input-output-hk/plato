package io.iohk.ethereum.timing

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.mining.ProofOfStakeMiner.StartMining
import io.iohk.ethereum.timing.BeaconActor.Start
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._


class BeaconActorSpec extends FlatSpec with Matchers {

  "The Beacon" should "immediately start with slot 1 upon system start-up" in new TestSetup() {
    (clockMock.now _).expects().returns(systemStartTime).repeat(3)

    beacon ! Start

    time.advance(1.millis) // almost 0
    minerMock.expectMsg(StartMining(1))
  }

  "The Beacon" should "immediately start with slot 1 when within the tolerance period" in new TestSetup() {
   (clockMock.now _).expects().returns(systemStartTime + 100.millis).repeat(3)

    beacon ! Start

    time.advance(1.millis) // almost 0
    minerMock.expectMsg(StartMining(1))
  }

  "The Beacon" should "skip slot 1 when outside the tolerance period" in new TestSetup() {
    (clockMock.now _).expects().returns(systemStartTime + 110.millis).repeat(3)

    beacon ! Start

    time.advance(slotDuration - 110.millis)
    minerMock.expectMsg(StartMining(2))
  }

  "The Beacon" should "start with slot N when started up just at start of said slot (N >= 2)" in new TestSetup() {
    (clockMock.now _).expects().returns(systemStartTime + 2.seconds).repeat(3) // just at the start of slot 2

    beacon ! Start

    time.advance(1.millis) // almost 0
    minerMock.expectMsg(StartMining(2))
  }

  "The Beacon" should "skip the current slot N when started in the middle of said slot (N >= 2)" in new TestSetup() {
    (clockMock.now _).expects().returns(systemStartTime + 3.seconds).repeat(3) // just in the middle of slot 2

    beacon ! Start

    time.advance(slotDuration - 1.second)
    minerMock.expectMsg(StartMining(3))
  }
}

class TestSetup extends MockFactory {

  implicit val system = ActorSystem("BeaconSpec_System")
  val minerMock = TestProbe()
  val slotDuration: FiniteDuration = 2.seconds
  val time = new VirtualTime
  val schedulerMock = time.scheduler
  val clockMock = mock[Clock]
  val systemStartTime = System.currentTimeMillis.millis
  val beacon = TestActorRef(Props(new BeaconActor(
    minerMock.ref, slotDuration, systemStartTime, Some(schedulerMock), Some(clockMock))))
}
