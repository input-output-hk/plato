package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.OuroborosConfig
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.duration._

class MinerStakeholdersConfigSpec extends FlatSpec with Matchers with PropertyChecks  {

  it should "return no stakeholder is allowed to mine if no entry matches" in new TestSetup {
    val allowedMinersSlot1 = MinerStakeholdersConfig.forSlot(1, ouroborosConfig)
    allowedMinersSlot1 shouldBe Nil
  }

  it should "return the set list of stakeholders if an entry matches" in new TestSetup {
    val allowedMinersSlot20 = MinerStakeholdersConfig.forSlot(20, ouroborosConfig)
    allowedMinersSlot20 shouldBe Seq(Address(2), Address(3))
  }

  it should "return the list of stakeholders from before the parameter slot if there's no perfect match" in new TestSetup {
    val allowedMinersSlot30 = MinerStakeholdersConfig.forSlot(25, ouroborosConfig)
    allowedMinersSlot30 shouldBe Seq(Address(2), Address(3))
  }

  it should "return an empty list of stakeholders if an entry matches and it contains and empty list" in new TestSetup {
    val allowedMinersSlot30 = MinerStakeholdersConfig.forSlot(30, ouroborosConfig)
    allowedMinersSlot30 shouldBe Nil
  }

  trait TestSetup {

    val minerStakeholdersMapping = Map(
      BigInt(10) -> Seq(Address(0), Address(1)),
      BigInt(20) -> Seq(Address(2), Address(3)),
      BigInt(30) -> Nil
    )

    val ouroborosConfig = new OuroborosConfig {
      override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = minerStakeholdersMapping

      // unused
      override val consensusContractAddress: Address = Address(0)
      override val consensusContractFilepath: String = ""
      override val slotDuration: FiniteDuration = 0.millis
    }
  }

}
