package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.Address
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class ElectionManagerImplSpec extends FlatSpec with Matchers with PropertyChecks with MockFactory {

  trait TestSetup {
    val knownStakeholders: Seq[Address] = List(
      Address(hexString = "0x01"),
      Address(hexString = "0x10"),
      Address(hexString = "0x11"))
  }

  "ElectionManagerImpl" should "choose only one stakeholder, given the order in the known stakeholder list" in new TestSetup {
    val electionManager: ElectionManager = ElectionManagerImpl(knownStakeholders)

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

    val electionManager: ElectionManager= ElectionManagerImpl(List.empty)

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
}
