package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.Address

trait ElectionManager {

  /**
    * Verifies whether the stakeholder is the leader corresponding to a given slot number.
    * FIXME: In the non mocked implementation it will be needed to access the relative stake of the
    *        stakeholder, this will be added when the implementation is changed
    *
    * @param stakeholderAddress that is checked whether it's the slot leader or not
    * @param slotNumber         that is checked whether the stakeholder is leader of it or not
    * @return whether the stakeholder is the leader of the slotNumber
    */
  def verifyIsLeader(stakeholderAddress: Address, slotNumber: BigInt): Boolean

}

// TODO: Implement an Election manager that uses Follow the Satoshi.
/**
  * This is a "mock" implementation for an ElectionManager, That choose a slot leader from a list
  * of know stakeholders fixed in the system using a "round robin" criteria.
  */
case class ElectionManagerImpl(knownStakeholders: Seq[Address]) extends ElectionManager {

  def verifyIsLeader(stakeholderAddress: Address, slotNumber: BigInt): Boolean = knownStakeholders.length match {
    case length if length > 0 => stakeholderAddress == knownStakeholders(((slotNumber - 1) % length).toInt)
    case _ => false
  }

}
