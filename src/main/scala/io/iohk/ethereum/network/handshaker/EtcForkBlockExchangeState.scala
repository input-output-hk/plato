package io.iohk.ethereum.network.handshaker

import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Logger
import scala.language.postfixOps
import scala.concurrent.duration._

case class EtcForkBlockExchangeState(handshakerConfiguration: EtcHandshakerConfiguration,
                                     forkResolver: ForkResolver, remoteStatus: Status) extends Handshaking with Logger {

  import handshakerConfiguration._

  def nextMessage: (Option[NextMessage], Handshaking) =
    (Some(NextMessage(
      messageToSend = GetBlockHeaders(Left(forkResolver.forkBlockNumber), maxHeaders = 1, skip = 0, reverse = false),
      timeout = peerConfiguration.waitForChainCheckTimeout
    )), this)

  def nextMessage(receivedMessage: Message): (Option[NextMessage], HandshakeState) = {

    receivedMessage match {
      case BlockHeaders(blockHeaders) =>

        val forkBlockHeaderOpt = blockHeaders.find(_.number == forkResolver.forkBlockNumber)

        forkBlockHeaderOpt match {
          case Some(forkBlockHeader) =>
            val fork = forkResolver.recognizeFork(forkBlockHeader)

            log.info("Peer is running the {} fork", fork)

            if (forkResolver.isAccepted(fork)) {
              log.info("Fork is accepted")
              val peerInfo: EtcPeerInfo = EtcPeerInfo(remoteStatus, remoteStatus.totalDifficulty, true, forkBlockHeader.number)
              (None, HandshakeSuccess(peerInfo))
            } else {
              log.warn("Fork is not accepted")
              (None, HandshakeFailure(Disconnect.Reasons.UselessPeer))
            }

          case None =>
            log.info("Peer did not respond with fork block header")
            (None, HandshakeSuccess(EtcPeerInfo(remoteStatus, remoteStatus.totalDifficulty, false, 0)))
        }

      case GetBlockHeaders(Left(number), numHeaders, _, _) if number == forkResolver.forkBlockNumber && numHeaders == 1 =>
        log.debug("Received request for fork block")
        blockchain.getBlockHeaderByNumber(number) match {
          case Some(header) => (Some(NextMessage(BlockHeaders(Seq(header)), 10 seconds)), this)
          case None => (Some(NextMessage(BlockHeaders(Nil), 10 seconds)), this)
        }

      case _ => (None, this)

    }

  }

  def processTimeout: (Option[NextMessage], HandshakeState) =
      (None, HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage))


}
