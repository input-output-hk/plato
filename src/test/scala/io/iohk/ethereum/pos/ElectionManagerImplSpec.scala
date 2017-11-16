package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.{Address, BlockchainImpl}
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

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 1, mock[BlockchainImpl]) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 1, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 1, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 2, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 2, mock[BlockchainImpl]) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 2, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 3, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 3, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 3, mock[BlockchainImpl]) shouldEqual true

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 4, mock[BlockchainImpl]) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 4, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 4, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 5, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 5, mock[BlockchainImpl]) shouldEqual true
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 5, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 6, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 6, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 6, mock[BlockchainImpl]) shouldEqual true
  }

  "ElectionManagerImpl" should "not choose any stakeholder, given an empty list of known stakeholder" in new TestSetup {

    val electionManager: ElectionManager= ElectionManagerImpl(List.empty)

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 1, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 1, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 1, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 2, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 2, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 2, mock[BlockchainImpl]) shouldEqual false

    electionManager.verifyIsLeader(knownStakeholders(0), slotNumber = 3, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(1), slotNumber = 3, mock[BlockchainImpl]) shouldEqual false
    electionManager.verifyIsLeader(knownStakeholders(2), slotNumber = 3, mock[BlockchainImpl]) shouldEqual false
  }
}
