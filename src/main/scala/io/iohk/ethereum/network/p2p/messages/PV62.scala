package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{BlockHeader, SignedBlockHeader, SignedTransaction}
import io.iohk.ethereum.network.p2p.{Message, MessageSerializableImplicit}
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{RLPList, _}
import org.spongycastle.util.encoders.Hex

object PV62 {
  object BlockHash {

    implicit class BlockHashEnc(blockHash: BlockHash) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(blockHash.hash, blockHash.number)
    }

    implicit class BlockHashDec(val bytes: Array[Byte]) extends AnyVal {
      def toBlockHash: BlockHash = BlockHashRLPEncodableDec(bytes).toBlockHash
    }

    implicit class BlockHashRLPEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
      def toBlockHash: BlockHash = rlpEncodeable match {
        case RLPList(hash, number) => BlockHash(hash, number)
        case _ => throw new RuntimeException("Cannot decode BlockHash")
      }
    }
  }

  case class BlockHash(hash: ByteString, number: BigInt) {
    override def toString: String = {
      s"""BlockHash {
         |hash: ${Hex.toHexString(hash.toArray[Byte])}
         |number: $number
         |}""".stripMargin
    }
  }

  object NewBlockHashes {

    val code: Int = Versions.SubProtocolOffset + 0x01

    implicit class NewBlockHashesEnc(val underlyingMsg: NewBlockHashes)
      extends MessageSerializableImplicit[NewBlockHashes](underlyingMsg) with RLPSerializable {

      import BlockHash._

      override def code: Int = NewBlockHashes.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.hashes.map(_.toRLPEncodable): _*)
    }

    implicit class NewBlockHashesDec(val bytes: Array[Byte]) extends AnyVal {
      import BlockHash._
      def toNewBlockHashes: NewBlockHashes = rawDecode(bytes) match {
        case rlpList: RLPList => NewBlockHashes(rlpList.items.map(_.toBlockHash))
        case _ => throw new RuntimeException("Cannot decode NewBlockHashes")
      }
    }
  }

  case class NewBlockHashes(hashes: Seq[BlockHash]) extends Message {
    override def code: Int = NewBlockHashes.code
  }

  object GetSignedBlockHeaders {
    val code: Int = Versions.SubProtocolOffset + 0x03

    implicit class GetSignedBlockHeadersEnc(val underlyingMsg: GetSignedBlockHeaders)
      extends MessageSerializableImplicit[GetSignedBlockHeaders](underlyingMsg) with RLPSerializable {

      override def code: Int = GetSignedBlockHeaders.code

      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        block match {
          case Left(blockNumber) => RLPList(blockNumber, maxHeaders, skip, if (reverse) 1 else 0)
          case Right(blockHash) => RLPList(blockHash, maxHeaders, skip, if (reverse) 1 else 0)
        }
      }
    }

    implicit class GetBlockHeadersDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetBlockHeaders: GetSignedBlockHeaders = rawDecode(bytes) match {
        case RLPList((block: RLPValue), maxHeaders, skip, reverse) if block.bytes.length < 32 =>
          GetSignedBlockHeaders(Left(block), maxHeaders, skip, (reverse: Int) == 1)

        case RLPList((block: RLPValue), maxHeaders, skip, reverse) =>
          GetSignedBlockHeaders(Right(block), maxHeaders, skip, (reverse: Int) == 1)

        case _ => throw new RuntimeException("Cannot decode GetBlockHeaders")
      }
    }
  }


  case class GetSignedBlockHeaders(block: Either[BigInt, ByteString], maxHeaders: BigInt, skip: BigInt, reverse: Boolean) extends Message {
    override def code: Int = GetSignedBlockHeaders.code

    override def toString: String = {
      s"""GetSignedBlockHeaders{
          |block: ${block.fold(a => a, b => Hex.toHexString(b.toArray[Byte]))}
          |maxHeaders: $maxHeaders
          |skip: $skip
          |reverse: $reverse
          |}
     """.stripMargin
    }
  }

  object SignedBlockHeaderImplicits {
    implicit class SignedBlockHeaderEnc(signedBlockHeader: SignedBlockHeader) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import signedBlockHeader._
        import signedBlockHeader.header._
        RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
          logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, slotNumber,
          signature.v, signature.r, signature.s)
      }
    }

    implicit class SignedBlockHeaderSeqEnc(blockHeaders: Seq[SignedBlockHeader]) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(blockHeaders.map(_.toRLPEncodable): _*)
    }
    implicit class SignedBlockHeaderDec(val bytes: Array[Byte]) extends AnyVal {
      def toSignedBlockHeader: SignedBlockHeader = SignedBlockHeaderEncodableDec(rawDecode(bytes)).toSignedBlockHeader
    }

    implicit class SignedBlockHeaderEncodableDec(val rlpEncodeable: RLPEncodeable) extends AnyVal {
      def toSignedBlockHeader: SignedBlockHeader = rlpEncodeable match {
        case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, slotNumber,
        pointSign, signatureRandom, signature) =>
          //FIXME: Block signature is not being validated when decoding the blocks, as being done with SignedTransaction
          SignedBlockHeader(
            BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
              logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, slotNumber),
            ECDSASignature(
              r = signatureRandom: BigInt,
              s = signature: BigInt,
              v = pointSign
            )
          )
      }
    }
  }

  object SignedBlockHeaders {

    val code: Int = Versions.SubProtocolOffset + 0x04

    implicit class SignedBlockHeadersEnc(val underlyingMsg: SignedBlockHeaders) extends
      MessageSerializableImplicit[SignedBlockHeaders](underlyingMsg) with RLPSerializable {
      import SignedBlockHeaderImplicits._

      override def code: Int = SignedBlockHeaders.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.headers.map(_.toRLPEncodable): _*)
    }

    implicit class SignedBlockHeadersDec(val bytes: Array[Byte]) extends AnyVal {
      import SignedBlockHeaderImplicits._

      def toSignedBlockHeaders: SignedBlockHeaders = rawDecode(bytes) match {
        case rlpList: RLPList => SignedBlockHeaders(rlpList.items.map(_.toSignedBlockHeader))
        case _ => throw new RuntimeException("Cannot decode SignedBlockHeaders")
      }
    }

  }

  case class SignedBlockHeaders(headers: Seq[SignedBlockHeader]) extends Message {
    override def code: Int = SignedBlockHeaders.code
  }

  object BlockBody {

    def blockBodyToRlpEncodable(
      blockBody: BlockBody,
      signedTxToRlpEncodable: SignedTransaction => RLPEncodeable,
      signedBlockHeaderToRlpEncodable: SignedBlockHeader => RLPEncodeable
    ): RLPEncodeable =
      RLPList(
        RLPList(blockBody.transactionList.map(signedTxToRlpEncodable): _*),
        RLPList(blockBody.uncleNodesList.map(signedBlockHeaderToRlpEncodable): _*)
      )

    implicit class BlockBodyEnc(msg: BlockBody) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
        import SignedBlockHeaderImplicits._
        blockBodyToRlpEncodable(
          msg,
          stx => SignedTransactionEnc(stx).toRLPEncodable,
          header => SignedBlockHeaderEnc(header).toRLPEncodable
        )
      }
    }

    implicit class BlockBlodyDec(val bytes: Array[Byte]) extends AnyVal {
      def toBlockBody: BlockBody = BlockBodyRLPEncodableDec(rawDecode(bytes)).toBlockBody
    }

    def rlpEncodableToBlockBody(
      rlpEncodeable: RLPEncodeable,
      rlpEncodableToSignedTransaction: RLPEncodeable => SignedTransaction,
      rlpEncodableToSignedBlockHeader: RLPEncodeable => SignedBlockHeader
    ): BlockBody =
      rlpEncodeable match {
        case RLPList((transactions: RLPList), (uncles: RLPList)) =>
          BlockBody(
            transactions.items.map(rlpEncodableToSignedTransaction),
            uncles.items.map(rlpEncodableToSignedBlockHeader)
          )
        case _ => throw new RuntimeException("Cannot decode BlockBody")
      }

    implicit class BlockBodyRLPEncodableDec(val rlpEncodeable: RLPEncodeable) {
      def toBlockBody: BlockBody = {
        import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions._
        import SignedBlockHeaderImplicits._
        rlpEncodableToBlockBody(
          rlpEncodeable,
          rlp => SignedTransactionRlpEncodableDec(rlp).toSignedTransaction,
          rlp => SignedBlockHeaderEncodableDec(rlp).toSignedBlockHeader
        )

      }
    }

  }

  case class BlockBody(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[SignedBlockHeader]) {
    override def toString: String =
      s"""BlockBody{
         |transactionList: $transactionList
         |uncleNodesList: $uncleNodesList
         |}
    """.stripMargin
  }

  object BlockBodies {

    val code: Int = Versions.SubProtocolOffset + 0x06

    implicit class BlockBodiesEnc(val underlyingMsg: BlockBodies) extends MessageSerializableImplicit[BlockBodies](underlyingMsg) with RLPSerializable {
      override def code: Int = BlockBodies.code

      override def toRLPEncodable: RLPEncodeable = RLPList(msg.bodies.map(_.toRLPEncodable): _*)
    }

    implicit class BlockBodiesDec(val bytes: Array[Byte]) extends AnyVal {
      import BlockBody._
      def toBlockBodies: BlockBodies = rawDecode(bytes) match {
        case rlpList: RLPList => BlockBodies(rlpList.items.map(_.toBlockBody))
        case _ => throw new RuntimeException("Cannot decode BlockBodies")
      }
    }
  }

  case class BlockBodies(bodies: Seq[BlockBody]) extends Message {
    val code: Int = BlockBodies.code
  }

  object GetBlockBodies {

    val code: Int = Versions.SubProtocolOffset + 0x05

    implicit class GetBlockBodiesEnc(val underlyingMsg: GetBlockBodies)
      extends MessageSerializableImplicit[GetBlockBodies](underlyingMsg) with RLPSerializable {

      override def code: Int = GetBlockBodies.code

      override def toRLPEncodable: RLPEncodeable = toRlpList(msg.hashes)
    }

    implicit class GetBlockBodiesDec(val bytes: Array[Byte]) extends AnyVal {
      def toGetBlockBodies: GetBlockBodies = rawDecode(bytes) match {
        case rlpList: RLPList => GetBlockBodies(fromRlpList[ByteString](rlpList))

        case _ => throw new RuntimeException("Cannot decode BlockHeaders")
      }
    }
  }

  case class GetBlockBodies(hashes: Seq[ByteString]) extends Message {
    override def code: Int = GetBlockBodies.code

    override def toString: String = {
      s"""GetBlockBodies {
         |hashes: ${hashes.map(h => Hex.toHexString(h.toArray[Byte]))}
         |}
     """.stripMargin
    }
  }
}
