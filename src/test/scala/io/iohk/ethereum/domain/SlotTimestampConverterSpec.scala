package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.OuroborosConfig
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.duration._

class SlotTimestampConverterSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "correctly obtain the slot beginning timestamp for each slot" in {

    val slotDurationMillis = 1000
    val ouroborosConfig = new OuroborosConfig {
      val slotDuration: Duration = slotDurationMillis.millis

      //unused
      val knownStakeholders: Seq[Address] = Nil
    }
    val genesisTimestamp = 1000

    val slotTimestampConverter = SlotTimestampConverter(ouroborosConfig, genesisTimestamp)

    val table = Table[Int, BigInt](
      ("slotNumber", "slotBeginning"),
      (0, 1000),
      (1, 2000),
      (10, 11000),
      (13, 14000)
    )

    forAll(table){ (slotNumber, slotBeginning) =>
      slotTimestampConverter.getSlotStartingTime(slotNumber) shouldBe slotBeginning
    }
  }

}
