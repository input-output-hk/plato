package io.iohk.ethereum.pos

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.utils.OuroborosConfig

import scala.util.Try

object MinerStakeholdersConfig {

  /**
    * Returns the list of stakeholders that are allowed to mine in a given slot
    *
    * @param slotNumber whose allowed stakeholders we wish to get
    * @param ouroborosConfig
    * @return a list of stakeholders that are allowed to mine in the given slot number
    */
  def forSlot(slotNumber: BigInt, ouroborosConfig: OuroborosConfig): Seq[Address] =
    Try {
      ouroborosConfig.slotMinerStakeholdersMapping.filterKeys(_ <= slotNumber).maxBy(_._1)._2
    }.getOrElse(Nil)

}
