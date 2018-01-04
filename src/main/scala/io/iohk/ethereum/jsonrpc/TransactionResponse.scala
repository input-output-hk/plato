package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.domain.{SignedBlockHeader, SignedTransaction}

case class TransactionResponse(
    hash: ByteString,
    nonce: BigInt,
    blockHash: Option[ByteString],
    blockNumber: Option[BigInt],
    transactionIndex: Option[BigInt],
    from: ByteString,
    to: Option[ByteString],
    value: BigInt,
    gasPrice: BigInt,
    gas: BigInt,
    input: ByteString,
    pending: Option[Boolean],
    isOutgoing: Option[Boolean])

object TransactionResponse {

  def apply(stx: SignedTransaction,
            blockHeader: Option[SignedBlockHeader] = None,
            transactionIndex: Option[Int] = None,
            pending: Option[Boolean] = None,
            isOutgoing: Option[Boolean] = None): TransactionResponse =
    TransactionResponse(
      hash = stx.hash,
      nonce = stx.tx.nonce,
      blockHash = blockHeader.map(_.hash),
      blockNumber = blockHeader.map(_.header.number),
      transactionIndex = transactionIndex.map(txIndex => BigInt(txIndex)),
      from = stx.senderAddress.bytes,
      to = stx.tx.receivingAddress.map(_.bytes),
      value = stx.tx.value,
      gasPrice = stx.tx.gasPrice,
      gas = stx.tx.gasLimit,
      input = stx.tx.payload,
      pending = pending,
      isOutgoing = isOutgoing)

}
