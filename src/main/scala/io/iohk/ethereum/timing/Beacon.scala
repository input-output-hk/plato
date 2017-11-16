package io.iohk.ethereum.timing

import akka.actor.{Actor, ActorLogging, ActorRef, Scheduler}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class Beacon(
  miner: ActorRef,
  slotDuration: FiniteDuration,
  externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor
    with ActorLogging {

  import Beacon._

  val startingSlotNumber: SlotNumber = 1
  val startingTime: Long = System.currentTimeMillis

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override def receive: Receive = idle

  def idle: Receive = {
    case Start =>
      context become start
      self ! NewSlot(startingSlotNumber)
    case _ => // nothing
  }

  private def start: Receive = {
    case NewSlot(currentSlotNumber) =>
      log.debug(s"Generating new slot $currentSlotNumber")

      import io.iohk.ethereum.mining.Miner._
      miner ! StartMining(currentSlotNumber)

      // FIXME: The use of longValue may cause precision loss.
      val nextSlotTime = startingTime + (slotDuration.toMillis * currentSlotNumber.longValue)
      val currentTime = System.currentTimeMillis
      val waitForNextSlot = FiniteDuration(nextSlotTime - currentTime, MILLISECONDS)

      log.debug(s"Next slot time is $nextSlotTime, current time is $currentTime, so waiting for $waitForNextSlot")

      scheduler.scheduleOnce(waitForNextSlot, self, NewSlot(currentSlotNumber + 1))
  }
}


object Beacon {

  case object Start
  case class NewSlot(slotNumber: SlotNumber)
}