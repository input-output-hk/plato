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

    onlySetStakeholderCanMine(knownStakeholders(0), knownStakeholders, slotNumber = 1, electionManager) shouldEqual true

    onlySetStakeholderCanMine(knownStakeholders(1), knownStakeholders, slotNumber = 2, electionManager) shouldEqual true

    onlySetStakeholderCanMine(knownStakeholders(2), knownStakeholders, slotNumber = 3, electionManager) shouldEqual true

    onlySetStakeholderCanMine(knownStakeholders(0), knownStakeholders, slotNumber = 4, electionManager) shouldEqual true

    onlySetStakeholderCanMine(knownStakeholders(1), knownStakeholders, slotNumber = 5, electionManager) shouldEqual true

    onlySetStakeholderCanMine(knownStakeholders(2), knownStakeholders, slotNumber = 6, electionManager) shouldEqual true
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
    val allStakeholders = knownStakeholders ++ otherKnownStakeholders

    // First round stakeholders
    onlySetStakeholderCanMine(knownStakeholders(0), allStakeholders, slotNumber = 1, electionManager) shouldEqual true
    onlySetStakeholderCanMine(knownStakeholders(1), allStakeholders, slotNumber = 2, electionManager) shouldEqual true
    onlySetStakeholderCanMine(knownStakeholders(2), allStakeholders, slotNumber = 3, electionManager) shouldEqual true

    // Second round stakeholders
    onlySetStakeholderCanMine(otherKnownStakeholders(1), allStakeholders, slotNumber = 4, electionManager) shouldEqual true
    onlySetStakeholderCanMine(otherKnownStakeholders(0), allStakeholders, slotNumber = 5, electionManager) shouldEqual true
    onlySetStakeholderCanMine(otherKnownStakeholders(1), allStakeholders, slotNumber = 6, electionManager) shouldEqual true
  }

  trait TestSetup {

    def onlySetStakeholderCanMine(miner: Address, stakeholders: Seq[Address],
                                  slotNumber: BigInt, electionManager: ElectionManager): Boolean =
      stakeholders.forall{ stakeholder =>
        val isSlotLeader = electionManager.verifyIsLeader(stakeholder, slotNumber)

        if(miner == stakeholder) isSlotLeader
        else !isSlotLeader
      }

    def configFromMinerStakeholderMapping(slotStakeholdersMapping: Map[BigInt, Seq[Address]]): OuroborosConfig =
      new OuroborosConfig {
        override val slotMinerStakeholdersMapping: Map[BigInt, Seq[Address]] = slotStakeholdersMapping

        //unused
        override val slotDuration: FiniteDuration = 0.millis
      }

    val knownStakeholders: Seq[Address] = List(Address(1), Address(2), Address(3))

    val otherKnownStakeholders: Seq[Address] = List(Address(3), Address(4))

    val noMinersAllowedConfig = configFromMinerStakeholderMapping(Map.empty)
    val fixedKnownStakeholdersConfig = configFromMinerStakeholderMapping(Map(BigInt(1) -> knownStakeholders))
    val changingMinersConfig = configFromMinerStakeholderMapping(Map(
      BigInt(1) -> knownStakeholders,
      BigInt(4) -> otherKnownStakeholders
    ))
  }
}
