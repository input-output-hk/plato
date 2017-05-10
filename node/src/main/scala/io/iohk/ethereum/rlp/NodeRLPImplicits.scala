package io.iohk.ethereum.rlp

import io.iohk.ethereum.rlp.RLP._
import io.iohk.ethereum.vm.UInt256

import scala.language.implicitConversions


object NodeRLPImplicits {

  implicit val uInt256EncDec =  new RLPEncoder[UInt256] with RLPDecoder[UInt256] {
    override def encode(obj: UInt256): RLPEncodeable =
      RLPValue(if (obj.equals(UInt256.Zero)) byteToByteArray(0: Byte) else obj.bytes.dropWhile(_ == 0).toArray[Byte])

    override def decode(rlp: RLPEncodeable): UInt256 = rlp match {
      case RLPValue(bytes) => UInt256(bytes)
      case _ => throw RLPException("src is not an RLPValue")
    }

  }

}
