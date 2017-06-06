package io.iohk.ethereum.network.handshaker

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Capability, Disconnect, Hello}
import io.iohk.ethereum.utils.{Config, Logger, ServerStatus}


case class EtcHelloExchangeState(handshakerConfiguration: EtcHandshakerConfiguration) extends Handshaking with Logger {

  import handshakerConfiguration._

  override def nextMessage: (Option[NextMessage], HandshakeState) = {
    log.info("RLPx connection established, sending Hello")
    (Some(NextMessage(
      messageToSend = createHelloMsg(),
      timeout = peerConfiguration.waitForHelloTimeout
    )), this)
  }

  def nextMessage(receivedMessage: Message): (Option[NextMessage], HandshakeState) = {
    receivedMessage match {
      case hello: Hello =>
        log.info("Protocol handshake finished with peer ({})", hello)
        if (hello.capabilities.contains(Capability("eth", Versions.PV63.toByte)))
          (None, EtcNodeStatusExchangeState(handshakerConfiguration))
        else {
          log.warn("Connected peer does not support eth {} protocol. Disconnecting.", Versions.PV63.toByte)
          (None, HandshakeFailure(Disconnect.Reasons.IncompatibleP2pProtocolVersion))
        }
      case _ => (None, HandshakeFailure(0)) // TODO REAL ERROR CODE REQUIRED
    }

  }

  override def processTimeout: (Option[NextMessage], HandshakeState) = {
    log.warn("Timeout while waiting for Hello")
      (None, HandshakeFailure(Disconnect.Reasons.TimeoutOnReceivingAMessage))
  }

  private def createHelloMsg(): Hello = {
    val nodeStatus = nodeStatusHolder()
    val listenPort = nodeStatus.serverStatus match {
      case ServerStatus.Listening(address) => address.getPort
      case ServerStatus.NotListening => 0
    }
    Hello(
      p2pVersion = EtcHelloExchangeState.P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Capability("eth", Versions.PV63.toByte)),
      listenPort = listenPort,
      nodeId = ByteString(nodeStatus.nodeId)
    )
  }
}

object EtcHelloExchangeState {
  val P2pVersion = 4
}
