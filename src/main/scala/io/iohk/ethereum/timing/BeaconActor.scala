package io.iohk.ethereum.timing

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}

import scala.collection.immutable.Stream
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

  private def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler
  private def clock: Clock = clockOpt getOrElse SystemClock

  private def prunePastSlots(slotList: SlotList, currentTime: FiniteDuration): SlotList = slotList.dropWhile {
    slot =>
      val timeDiff = currentTime - slot.startTime
      if (slot.number == startingSlotNumber)
        // The starting slot should be skipped only when outside the tolerance period.
        timeDiff > startingSlotTolerance
      else
        // The Beacon started after slot N started, so skip it (with N not being the starting slot number).
        timeDiff > 0.millis
    }

  override def receive: Receive = idle

  private def idle: Receive = {
    case Start =>
      log.debug(s"Beacon started")

      val slotList = slots(systemStartTime, slotDuration)
      val prunedList = prunePastSlots(slotList, clock.now())

      context become start
      self ! RemainingSlots(prunedList)
    case _ => // nothing
  }

  private def start: Receive = {
    case RemainingSlots(slotList) =>
      val slot = slotList.head
      log.debug(s"Processing slot ${slot.number}")

      val currentTime = clock.now
      val delay = 0.millis max slot.startTime - currentTime
      log.debug(s"Slot start time is ${slot.startTime}, current time is $currentTime, so waiting for $delay")

      scheduler.scheduleOnce(delay) { signalMiner(slot.number, slotList) }
  }

  private def signalMiner(slotNumber: BigInt, slotList: SlotList): Unit = {
    import io.iohk.ethereum.mining.ProofOfStakeMiner.StartMining
    miner ! StartMining(slotNumber)
    self ! RemainingSlots(slotList.tail)
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

  val startingSlotNumber = 1

  // NOTE: A tolerance period is allowed in order for the starting slot to be generated when the Beacon starts up a
  // little bit later than the system (which is expected).
  // TODO: Make the tolerance period a configurable parameter.
  val startingSlotTolerance = 100.millis

  case class Slot(number: BigInt, startTime: FiniteDuration)

  type SlotList = Stream[Slot]

  def slots(systemStartTime: FiniteDuration, slotDuration: FiniteDuration): SlotList = {
    def loop(slotNumber: BigInt, slotStartTime: FiniteDuration, slotDuration: FiniteDuration): SlotList = {
      Slot(slotNumber, slotStartTime) #:: loop(slotNumber + 1, slotStartTime + slotDuration, slotDuration)
    }
    loop(startingSlotNumber, systemStartTime, slotDuration)
  }

  case object Start
  case class RemainingSlots(slots: SlotList)
}
