package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.OuroborosConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.concurrent.duration._

class ElectionManagerImplSpec extends FlatSpec with Matchers with PropertyChecks with MockFactory {

  "ElectionManagerImpl" should "choose only one stakeholder, given the order in the known stakeholder list" in new TestSetup {
    val electionManager: ElectionManager = ElectionManagerImpl(fixedKnownStakeholdersConfig)

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 1) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 1) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 1) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 2) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 2) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 2) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 3) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 3) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 3) shouldEqual true

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 4) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 4) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 4) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 5) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 5) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 5) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 6) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 6) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 6) shouldEqual true
  }

  "ElectionManagerImpl" should "not choose any stakeholder, given an empty list of known stakeholder" in new TestSetup {

    val electionManager: ElectionManager= ElectionManagerImpl(noMinersAllowedConfig)

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 1) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 1) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 1) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 2) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 2) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 2) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 3) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 3) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 3) shouldEqual false
  }

  "ElectionManagerImpl" should "choose the correct miner, given a change in the stakeholder list" in new TestSetup {

    val electionManager: ElectionManager= ElectionManagerImpl(changingMinersConfig)

    // First round stakeholders
    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 1) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 2) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 3) shouldEqual true

    // Second round stakeholders
    electionManager.verifyIsLeader(otherKnownStakeholders(1), slotNumber = 4) shouldEqual true
    electionManager.verifyIsLeader(otherKnownStakeholders(0), slotNumber = 5) shouldEqual true
    electionManager.verifyIsLeader(otherKnownStakeholders(1), slotNumber = 6) shouldEqual true
  }

  trait TestSetup {

    def configFromMinerStakeholderMapping(slotStakeholdersMapping: Map[BigInt, Seq[Address]]): OuroborosConfig =
      new OuroborosConfig {
        override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = slotStakeholdersMapping

        //unused
        override val slotDuration: FiniteDuration = 0.millis
      }

    val knownStakeholders: Seq[Address] = List(
      Address(hexString = "0x01"),
      Address(hexString = "0x10"),
      Address(hexString = "0x11"))

    val otherKnownStakeholders: Seq[Address] = List(Address(100), Address(200))

    val noMinersAllowedConfig = configFromMinerStakeholderMapping(Map.empty)
    val fixedKnownStakeholdersConfig = configFromMinerStakeholderMapping(Map(BigInt(1) -> knownStakeholders))
    val changingMinersConfig = configFromMinerStakeholderMapping(Map(
      BigInt(1) -> knownStakeholders,
      BigInt(4) -> otherKnownStakeholders
    ))
  }
}
