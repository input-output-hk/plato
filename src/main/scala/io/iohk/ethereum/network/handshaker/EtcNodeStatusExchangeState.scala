package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger

case class EtcNodeStatusExchangeState(handshakerConfiguration: EtcHandshakerConfiguration) extends InProgressState[PeerInfo] with Logger {

  import handshakerConfiguration._

  def nextMessage: NextMessage =
    NextMessage(
      messageToSend = createStatusMsg(),
      timeout = peerConfiguration.waitForStatusTimeout
    )

  def applyResponseMessage: PartialFunction[Message, HandshakerState[PeerInfo]] = {

    case remoteStatus: Status =>
      log.debug("Peer returned status ({})", remoteStatus)

      forkResolverOpt match {
        case Some(forkResolver) =>
          EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, remoteStatus)
        case None =>
          ConnectedState(PeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, 0))
      }

  }

  def processTimeout: HandshakerState[PeerInfo] = {
    log.debug("Timeout while waiting status")
    DisconnectedState(Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    blockchain.getSignedBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisSignedHeader)
  }

  private def createStatusMsg(): Status = {
    val bestBlockHeader = getBestBlockHeader()
    val status = Status(
      protocolVersion = Versions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = bestBlockHeader.header.difficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisSignedHeader.hash)
    log.debug(s"sending status $status")
    status
  }

}
