package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.OuroborosConfig
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.duration._

class SlotTimeConverterSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "correctly obtain the slot beginning timestamp for each slot greater than 0" in {

    val slotDurationMillis = 1000
    val ouroborosConfig = new OuroborosConfig {
      val slotDuration: FiniteDuration = slotDurationMillis.millis

      //unused
      val knownStakeholders: Seq[Address] = Nil
    }
    val slot1StartingTime = 1.seconds

    val slotTimestampConverter = SlotTimeConverter(ouroborosConfig, slot1StartingTime)

    val table = Table[Int, Int](
      ("slotNumber", "slotBeginningSeconds"),
      (1, 1000),
      (2, 2000),
      (10, 10000),
      (13, 13000)
    )

    forAll(table){ (slotNumber, slotBeginningSeconds) =>
      slotTimestampConverter.getSlotStartingMillis(slotNumber) shouldBe slotBeginningSeconds.seconds.toMillis
    }
  }

}
