package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.OuroborosConfig
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.duration._

class SlotTimeConverterSpec extends FlatSpec with Matchers with PropertyChecks {

  it should "correctly obtain the slot beginning timestamp for each slot greater than 0" in {

    val ouroborosConfig = new OuroborosConfig {
      override val slotDuration: FiniteDuration = 2.seconds

      //unused
      override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = Map.empty
    }
    val slot1StartingTime = 1.seconds

    val slotTimestampConverter = SlotTimeConverter(ouroborosConfig, slot1StartingTime)

    val table = Table[Int, Int](
      ("slotNumber", "slotBeginningSeconds"),
      (1, 1),
      (2, 3),
      (10, 19),
      (13, 25)
    )

    forAll(table){ (slotNumber, slotBeginningSeconds) =>
      slotTimestampConverter.getSlotStartingMillis(slotNumber) shouldBe slotBeginningSeconds.seconds.toMillis
    }
  }

}
