package io.iohk.ethereum.timing

import scala.concurrent.duration.FiniteDuration


trait Clock {

  /**
    * Returns the current time in milliseconds.
    * @return the difference, measured in milliseconds, between the current time and the Unix epoch.
    */
  def now(): FiniteDuration
}
