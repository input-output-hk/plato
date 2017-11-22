package io.iohk.ethereum.timing

import io.iohk.ethereum.utils.NTP

import scala.concurrent.duration._

case class SystemClock(ntp: NTP) extends Clock {
  override def now(): FiniteDuration = ntp.correctedTime().millis
}
