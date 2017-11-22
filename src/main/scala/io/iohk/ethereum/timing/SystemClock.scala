package io.iohk.ethereum.timing

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}


object SystemClock extends Clock {
  override def now(): FiniteDuration = FiniteDuration(System.currentTimeMillis(), MILLISECONDS)
}
