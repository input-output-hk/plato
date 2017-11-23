package io.iohk.ethereum.domain

import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.{RLPEncodeable, RLPList, RLPSerializable, rawDecode}

/**
  * This class represent a block as a header and a body which are returned in two different messages
  *
  * @param signedHeader Block header
  * @param body   Block body
  */
case class Block(signedHeader: SignedBlockHeader, body: BlockBody) {
  override def toString: String = {
    s"""BlockHeader {
       | header: $signedHeader
       | body: $body
     """.stripMargin
  }

  def idTag: String =
    signedHeader.idTag
}

object Block {

  implicit class BlockEnc(val obj: Block) extends RLPSerializable {
    import io.iohk.ethereum.network.p2p.messages.PV62.SignedBlockHeaderImplicits._
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._

    override def toRLPEncodable: RLPEncodeable =  RLPList(
      obj.signedHeader.toRLPEncodable,
      RLPList(obj.body.transactionList.map(_.toRLPEncodable): _*),
      RLPList(obj.body.uncleNodesList.map(_.toRLPEncodable): _*)
    )
  }

  implicit class BlockDec(val bytes: Array[Byte]) extends AnyVal {
    import io.iohk.ethereum.network.p2p.messages.PV62.SignedBlockHeaderImplicits._
    import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
    def toBlock: Block = rawDecode(bytes) match {
      case RLPList(header: RLPList, stx: RLPList, uncles: RLPList) => Block(
        header.toSignedBlockHeader,
        BlockBody(
          stx.items.map(_.toSignedTransaction),
          uncles.items.map(_.toSignedBlockHeader)
        )
      )
      case _ => throw new RuntimeException("Cannot decode block")
    }
  }

  def size(block: Block): Long = (block.toBytes: Array[Byte]).length
}
