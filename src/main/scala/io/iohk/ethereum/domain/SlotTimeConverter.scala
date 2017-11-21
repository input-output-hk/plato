package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.OuroborosConfig

import scala.concurrent.duration._

/**
  * The slot 1 starting time should be set to be the timestamp of the genesis block
  */
case class SlotTimeConverter(ouroborosConfig: OuroborosConfig, slot1StartingTime: FiniteDuration) {

  /**
    * Returns the number of milliseconds at which the slot begins.
    * Slot 1 begins at the slot1StartingTime, with each slot lasting slotDuration
    *
    * @param slotNumber whose starting time will be calculated. It should be greater than 0
    * @return the starting time in milliseconds corresponding to the slot number parameter
    */
  def getSlotStartingMillis(slotNumber: BigInt): BigInt = {
    require(slotNumber >= 1)
    (slotNumber - 1) * ouroborosConfig.slotDuration.toMillis + slot1StartingTime.toMillis
  }

}
