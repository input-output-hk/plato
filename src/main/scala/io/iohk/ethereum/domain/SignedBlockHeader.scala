package io.iohk.ethereum.domain

import java.math.BigInteger
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.mpt.ByteArraySerializable
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.{encode => rlpEncode, _}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.network.p2p.messages.PV62.SignedBlockHeaderImplicits._

/**
  * FIXME:
  * As a first implementation for the SignedBlockHeader we decide to not remove the beneficiary address
  * from the block header in order to reduce the impact in the client,
  * but as a future improvement we should remove it and add it into the SignedBlockHeader.
  * This also implies two implement two different RLP encoders one without the beneficiary,
  * used when the block is broadcast, and other with it, that is used when we store the block
  * (emulating the behaviour that we have already used for txs and signed txs).
  */

case class SignedBlockHeader (
                               header: BlockHeader,
                               signature: ECDSASignature) {

  override def toString: String = {
    s"""SignedTransaction {
       |blockHeader: $header
       |signature: $signature
       |}""".stripMargin
  }

  lazy val hash: ByteString = ByteString(kec256(this.toBytes : Array[Byte]))
  lazy val hashAsHexString: String = Hex.toHexString(hash.toArray[Byte])

  def idTag: String =  s"$header.number: $hashAsHexString"
}

object SignedBlockHeader {

  val FirstByteOfAddress = 12
  val LastByteOfAddress: Int = FirstByteOfAddress + Address.Length
  val negativePointSign = 27
  val newNegativePointSign = 35
  val positivePointSign = 28
  val newPositivePointSign = 36
  val valueForEmptyR = 0
  val valueForEmptyS = 0

  def apply(blockHeader: BlockHeader, pointSign: Byte, signatureRandom: ByteString, signature: ByteString): Option[SignedBlockHeader] = {
    val blockHeaderSignature = ECDSASignature(r = new BigInteger(1, signatureRandom.toArray), s = new BigInteger(1, signature.toArray), v = pointSign)
    for {
      coinbase <- SignedBlockHeader.getSender(blockHeader, blockHeaderSignature)
      if coinbase == Address(blockHeader.beneficiary)
    } yield SignedBlockHeader(blockHeader, blockHeaderSignature)
  }

  def apply(tx: Transaction, pointSign: Byte, signatureRandom: ByteString, signature: ByteString, address: Address): SignedTransaction = {
    val txSignature = ECDSASignature(r = new BigInteger(1, signatureRandom.toArray), s = new BigInteger(1, signature.toArray), v = pointSign)
    SignedTransaction(tx, txSignature, address)
  }

  def sign(blockHeader: BlockHeader, keyPair: AsymmetricCipherKeyPair): SignedBlockHeader = {
    val bytes = SignedBlockHeader.bytesToSign(blockHeader)
    val sig = ECDSASignature.sign(bytes, keyPair)
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val pub = keyPair.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    SignedBlockHeader(blockHeader, sig)
  }

  private def getSender(blockHeader: BlockHeader, signature: ECDSASignature): Option[Address] = {
    val bytesToSign: Array[Byte] = SignedBlockHeader.bytesToSign(blockHeader)
    val recoveredPublicKey: Option[Array[Byte]] = signature.publicKey(bytesToSign)
    for {
      key <- recoveredPublicKey
      addrBytes = crypto.kec256(key).slice(FirstByteOfAddress, LastByteOfAddress)
      if addrBytes.length == Address.Length
    } yield Address(addrBytes)
  }

  private def bytesToSign(blockHeader: BlockHeader): Array[Byte] = {
    import blockHeader._
    crypto.kec256(
      rlpEncode(
        RLPList(
          parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
          logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce, slotNumber
        )
      )
    )
  }

  val byteArraySerializable = new ByteArraySerializable[SignedBlockHeader] {

    override def fromBytes(bytes: Array[Byte]): SignedBlockHeader = bytes.toSignedBlockHeader

    override def toBytes(input: SignedBlockHeader): Array[Byte] = input.toBytes
  }
}