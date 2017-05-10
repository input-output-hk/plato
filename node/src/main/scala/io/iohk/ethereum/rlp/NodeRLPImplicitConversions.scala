package io.iohk.ethereum.rlp


import io.iohk.ethereum.rlp.RLPImplicitConversions.fromEncodeable
import io.iohk.ethereum.vm.UInt256
import io.iohk.ethereum.rlp.NodeRLPImplicits._
import scala.language.implicitConversions

object NodeRLPImplicitConversions {
  implicit def uInt256FromEncodeable: (RLPEncodeable) => UInt256 = fromEncodeable[UInt256]
}
