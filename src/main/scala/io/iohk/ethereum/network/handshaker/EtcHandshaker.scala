package io.iohk.ethereum.network.handshaker

import akka.agent.Agent
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.utils.NodeStatus

object EtcHandshaker {

  def apply(handshakerConfiguration: EtcHandshakerConfiguration): HandshakeState = {
    EtcHelloExchangeState(handshakerConfiguration)
  }

}

trait EtcHandshakerConfiguration {
  val nodeStatusHolder: Agent[NodeStatus]
  val blockchain: Blockchain
  val appStateStorage: AppStateStorage
  val peerConfiguration: PeerConfiguration
  val forkResolverOpt: Option[ForkResolver]
}
