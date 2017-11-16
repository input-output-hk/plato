package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.OuroborosConfig

case class SlotTimestampConverter(ouroborosConfig: OuroborosConfig, genesisBlockTimestamp: BigInt) {

  def getSlotStartingTime(slotNumber: BigInt): BigInt =
    slotNumber * ouroborosConfig.slotDuration.toMillis + genesisBlockTimestamp

}
