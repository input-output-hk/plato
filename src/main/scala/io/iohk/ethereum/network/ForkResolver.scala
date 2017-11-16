package io.iohk.ethereum.network

import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.DaoForkConfig

trait ForkResolver {
  type Fork <: ForkResolver.Fork

  def forkBlockNumber: BigInt
  def recognizeFork(blockHeader: BlockHeader): Fork
  def isAccepted(fork: Fork): Boolean
}

/**
  * As the will be no DAO fork block on Ouroboros, the ForkResolver was set to return that the peer is
  * always "on the same side of the fork" as our node.
  * FIXME: Remove it and it's usages.
  */
object ForkResolver {

  trait Fork

  class EtcForkResolver(daoForkConfig: DaoForkConfig) extends ForkResolver {
    sealed trait Fork extends ForkResolver.Fork
    case object AcceptedFork extends Fork
    case object RejectedFork extends Fork

    override def forkBlockNumber: BigInt = daoForkConfig.forkBlockNumber

    override def recognizeFork(blockHeader: BlockHeader): Fork = AcceptedFork

    override def isAccepted(fork: Fork): Boolean = fork == AcceptedFork
  }

}
