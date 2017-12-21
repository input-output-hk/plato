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
      override val consensusContractAddress: Address = Address(0)
      override val consensusContractFilepath: String = ""
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

  it should "correctly obtain the slot number for each timestamp" in {

    val ouroborosConfig = new OuroborosConfig {
      override val slotDuration: FiniteDuration = 2.seconds

      //unused
      override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = Map.empty
      override val consensusContractAddress: Address = Address(0)
      override val consensusContractFilepath: String = ""
    }
    val slot1StartingTime = 1.seconds

    val slotTimestampConverter = SlotTimeConverter(ouroborosConfig, slot1StartingTime)

    val table = Table[FiniteDuration, Int](
      ("timestamp", "slotNumber"),
      //Timestamp at slot beginning
      (1.seconds, 1),
      (3.seconds, 2),
      (19.seconds, 10),
      (25.seconds, 13),

      //Timestamp before slot 1
      (0.5.seconds, 0),

      //Timestamp in between slots
      (4.seconds, 2),
      (20.seconds, 10),
      (25.5.seconds, 13)
    )

    forAll(table){ (timestamp, slotNumber) =>
      slotTimestampConverter.getSlotNumberFromTime(timestamp) shouldBe slotNumber
    }
  }

}
