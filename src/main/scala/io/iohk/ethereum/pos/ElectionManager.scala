package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.{Address, Blockchain}

trait ElectionManager {

  def verifyIsLeader(stakeholder: Address, slotNumber: BigInt, blockchain: Blockchain): Boolean

}
