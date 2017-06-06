package io.iohk.ethereum.network.handshaker


import io.iohk.ethereum.network.p2p.{Message, MessageSerializable}

import scala.concurrent.duration.FiniteDuration

sealed trait HandshakeState

sealed trait HandshakeComplete extends HandshakeState
case class HandshakeFailure(reason: Int) extends HandshakeComplete
case class HandshakeSuccess[T](result: T) extends HandshakeComplete
case class NextMessage(messageToSend: MessageSerializable, timeout: FiniteDuration)


trait Handshaking extends HandshakeState {

  /**
    * Obtains the next message to be sent if the handshaking is in progress, or the result of the handshake
    *
    * @return next message to be sent or the result of the handshake
    */
  def nextMessage(receivedMessage: Message): (Option[NextMessage], HandshakeState)
  def nextMessage: (Option[NextMessage], HandshakeState)

  /**
    * Processes a timeout to the latest message sent and obtains the new Handshaker
    *
    * @return handshaker after the timeout was processed
    */
  def processTimeout: (Option[NextMessage], HandshakeState)

}
