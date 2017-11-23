package io.iohk.ethereum.utils

import java.net.InetAddress

import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

import scala.concurrent.duration._

/**
  * @note NTP implementation copied from Scorex:
  *       https://github.com/input-output-hk/Scorex/blob/master/scorex-basics/src/main/scala/scorex/utils/NTP.scala
  */
class NTPService(ntpConfig: NTPServiceConfig) extends Logger {
  private val TimeTillUpdateMillis = ntpConfig.updateOffsetInterval.toMillis
  private val NTPServer = ntpConfig.ntpServer
  private val NTPClientDefaultTimeout = 10000

  private var lastUpdate = 0L
  private var offset = 0L

  def correctedTime(): FiniteDuration = {
    //CHECK IF OFFSET NEEDS TO BE UPDATED
    if (System.currentTimeMillis() > lastUpdate + TimeTillUpdateMillis) {
      Try {
        updateOffset()

        log.debug("Adjusting time with " + offset + " milliseconds.")
      } recover {
        case e: Throwable =>
          log.warn("Unable to get corrected time", e)
      }
    }

    //CALCULATE CORRECTED TIME
    (System.currentTimeMillis() + offset).millis
  }

  private def updateOffset() {
    val client = new NTPUDPClient()
    client.setDefaultTimeout(NTPClientDefaultTimeout)

    try {
      client.open()

      val info = client.getTime(InetAddress.getByName(NTPServer))
      info.computeDetails()
      if (Option(info.getOffset).isDefined) offset = info.getOffset
    } catch {
      case t: Throwable => log.warn("Problems with NTP: ", t)
    } finally {
      client.close()
    }

    lastUpdate = System.currentTimeMillis()
  }
}
