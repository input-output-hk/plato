package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger

case class EtcNodeStatusExchangeState(handshakerConfiguration: EtcHandshakerConfiguration) extends Handshaking with Logger {

  import handshakerConfiguration._

  def createNextMessage: NextMessage =
    NextMessage(
      messageToSend = createStatusMsg(),
      timeout = peerConfiguration.waitForStatusTimeout
    )

  override def nextMessage: (Option[NextMessage], HandshakeState) = (Some(createNextMessage), this)

  override def nextMessage(receivedMessage: Message): (Option[NextMessage], HandshakeState) = {

    receivedMessage match {
    case remoteStatus: Status =>
      log.info("Peer returned status ({})", remoteStatus)

      forkResolverOpt match {
        case Some(forkResolver) =>
          (None, EtcForkBlockExchangeState(handshakerConfiguration, forkResolver, remoteStatus))
        case None =>
          (None, HandshakeSuccess(EtcPeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, 0)))

      }
    }

  }

  def processTimeout: (Option[NextMessage], HandshakeState) = {
    log.warn("Timeout while waiting status")
    (None, HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage))
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    blockchain.getBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisHeader)
  }

  private def createStatusMsg(): Status = {
    val bestBlockHeader = getBestBlockHeader()
    Status(
      protocolVersion = Versions.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = bestBlockHeader.difficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash)
  }

}
