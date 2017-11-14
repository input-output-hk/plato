package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.{Address, Blockchain}

trait ElectionManager {

  def verifyIsLeader(publicKey: Address, slotNumber: BigInt, blockchain: Blockchain): Boolean

}

// TODO: Implement an Election manager that use follow the satoshi.

/**
  * This is a "mock" implementation for an ElectionManager, That choose a slot leader from a list
  * of know stakeholders fixed in the system using a "round robin" criteria.
  */

case class ElectionManagerImpl(knownStakeholders: Seq[Address]) extends ElectionManager {

  def verifyIsLeader(publicKey: Address, slotNumber: BigInt, blockchain: Blockchain): Boolean = knownStakeholders.length match {
    case length if length > 0 => publicKey == knownStakeholders(((slotNumber - 1) % length).toInt)
    case _ => false
  }

}