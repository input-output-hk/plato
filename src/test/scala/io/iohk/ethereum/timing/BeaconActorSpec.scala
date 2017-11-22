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
    val systemStartTime = System.currentTimeMillis.millis
    val beacon = TestActorRef(Props(new BeaconActor(
      minerMock.ref, slotDuration, systemStartTime, Some(schedulerMock), Some(clockMock))))

    (clockMock.now _).expects().returns(systemStartTime).repeat(3)

    beacon ! Start

    time.advance(slotDuration)
    minerMock.expectMsg(StartMining(1))
  }

  /*
  "The Beacon" should "start with the correct slot number when started up after system start-up" in new TestSetup() {
    val systemStartTime = System.currentTimeMillis.millis - 4.seconds
    val beacon = TestActorRef(Props(new BeaconActor(
      minerMock.ref, slotDuration, systemStartTime, Some(schedulerMock))))
    beacon ! Start
    minerMock.expectMsg(StartMining(3))
  }

  "The Beacon" should "wait until the time comes to start with slot 1 when system start-up lies in the future" in new TestSetup() {
    val systemStartTime = System.currentTimeMillis.millis + 10.minutes
    val beacon = TestActorRef(Props(new BeaconActor(
      minerMock.ref, slotDuration, systemStartTime, Some(schedulerMock))))
    beacon ! Start
    minerMock.expectMsg(StartMining(1))
  }
*/

  /* TODO: Add the other tests:
  - Beacon signals the Miner at the exact slot start time when no delay in the scheduler is present
  - Beacon signals the Miner at the exact slot start time in spite of reasonable delays in the scheduler
  Scenario 1: Beacon starts upon system startup
    - correctly generates slot N+1 after D seconds have passed, where D is the slot duration
  Scenario 2: Beacon starts later
    - if in the middle of a slot N, then wait until N+1 and send message for slot N+1
  */

}

class TestSetup extends MockFactory {

  implicit val system = ActorSystem("BeaconSpec_System")
  val minerMock = TestProbe()
  val slotDuration: FiniteDuration = 2.seconds
  val time = new VirtualTime
  val schedulerMock = time.scheduler
  val clockMock = mock[Clock]
}
