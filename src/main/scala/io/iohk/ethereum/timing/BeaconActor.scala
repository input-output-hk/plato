package io.iohk.ethereum.timing

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class BeaconActor(
  miner: ActorRef,
  slotDuration: FiniteDuration,
  systemStartTime: FiniteDuration = System.currentTimeMillis.millis,
  externalSchedulerOpt: Option[Scheduler] = None,
  clockOpt: Option[Clock] = None)
  extends Actor
    with ActorLogging {

  import BeaconActor._

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler
  def clock: Clock = clockOpt getOrElse SystemClock

  private def calculateStartingSlot(): BigInt = {
    val currentTime = clock.now
    val elapsedTimeSinceStartTime = currentTime - systemStartTime
    // FIXME: When the Beacon is started in the middle of a slot N, it should schedule the slot N+1 taking into account
    // the time left to end the current slot N.
    (elapsedTimeSinceStartTime.toMillis / slotDuration.toMillis) + 1
  }

  override def receive: Receive = idle

  def idle: Receive = {
    case Start =>
      log.debug(s"Beacon started")
      context become start
      val startingSlotNumber = calculateStartingSlot()
      self ! NewSlot(startingSlotNumber)
    case _ => // nothing
  }

  private def start: Receive = {
    case NewSlot(currentSlotNumber) =>
      log.debug(s"Generating new slot $currentSlotNumber")

      import io.iohk.ethereum.mining.ProofOfStakeMiner._
      miner ! StartMining(currentSlotNumber)

      // FIXME: The use of toLong may cause precision loss.
      val nextSlotTime = systemStartTime.toMillis + (slotDuration.toMillis * currentSlotNumber.toLong)
      val currentTime = clock.now
      val waitForNextSlot = FiniteDuration(nextSlotTime - currentTime.toMillis, MILLISECONDS)

      log.debug(s"Next slot time is $nextSlotTime, current time is $currentTime, so waiting for $waitForNextSlot")

      scheduler.scheduleOnce(waitForNextSlot, self, NewSlot(currentSlotNumber + 1))
  }
}


object BeaconActor {

  def props(
    miner: ActorRef,
    slotDuration: FiniteDuration,
    systemStartTime: FiniteDuration = System.currentTimeMillis.millis,
    externalSchedulerOpt: Option[Scheduler] = None,
    clockOpt: Option[Clock] = None): Props =
    Props(new BeaconActor(
      miner,
      slotDuration,
      systemStartTime,
      externalSchedulerOpt,
      clockOpt)
    )

  case object Start
  case class NewSlot(slotNumber: BigInt)
}