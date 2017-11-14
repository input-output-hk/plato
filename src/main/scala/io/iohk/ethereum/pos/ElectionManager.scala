package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.{Address, Blockchain}

trait ElectionManager {

  def verifyIsLeader(publicKey: Address, slotNumber: BigInt, blockchain: Blockchain): Boolean

}