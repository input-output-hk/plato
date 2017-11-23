package io.iohk.ethereum.timing

import io.iohk.ethereum.utils.NTPService

import scala.concurrent.duration._

case class NTPClock(ntpService: NTPService) extends Clock {
  override def now(): FiniteDuration = ntpService.correctedTime()
}
