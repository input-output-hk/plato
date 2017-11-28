package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.{Block, SignedBlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

case class BlockResponse(
    number: BigInt,
    hash: Option[ByteString],
    parentHash: ByteString,
    nonce: Option[ByteString],
    sha3Uncles: ByteString,
    logsBloom: ByteString,
    transactionsRoot: ByteString,
    stateRoot: ByteString,
    receiptsRoot: ByteString,
    miner: Option[ByteString],
    difficulty: BigInt,
    totalDifficulty: Option[BigInt],
    extraData: ByteString,
    size: BigInt,
    gasLimit: BigInt,
    gasUsed: BigInt,
    timestamp: BigInt,
    transactions: Either[Seq[ByteString], Seq[TransactionResponse]],
    uncles: Seq[ByteString],
    signature: ECDSASignature)

object BlockResponse {

  def apply(block: Block, totalDifficulty: Option[BigInt] = None,
            fullTxs: Boolean = false, pendingBlock: Boolean = false): BlockResponse = {
    val transactions =
      if (fullTxs)
        Right(block.body.transactionList.zipWithIndex.map { case (stx, transactionIndex) =>
          TransactionResponse(stx = stx, blockHeader = Some(block.signedHeader), transactionIndex = Some(transactionIndex))
        })
      else
        Left(block.body.transactionList.map(_.hash))

    BlockResponse(
      number = block.signedHeader.header.number,
      hash = if(pendingBlock) None else Some(block.signedHeader.hash),
      parentHash = block.signedHeader.header.parentHash,
      nonce = if(pendingBlock) None else Some(block.signedHeader.header.nonce),
      sha3Uncles = block.signedHeader.header.ommersHash,
      logsBloom = block.signedHeader.header.logsBloom,
      transactionsRoot = block.signedHeader.header.transactionsRoot,
      stateRoot = block.signedHeader.header.stateRoot,
      receiptsRoot = block.signedHeader.header.receiptsRoot,
      miner = if(pendingBlock) None else Some(block.signedHeader.header.beneficiary),
      difficulty = block.signedHeader.header.difficulty,
      totalDifficulty = totalDifficulty,
      extraData = block.signedHeader.header.extraData,
      size = Block.size(block),
      gasLimit = block.signedHeader.header.gasLimit,
      gasUsed = block.signedHeader.header.gasUsed,
      timestamp = block.signedHeader.header.unixTimestamp,
      transactions = transactions,
      uncles = block.body.uncleNodesList.map(_.hash),
      signature = block.signedHeader.signature
    )
  }

  def apply(signedBlockHeader: SignedBlockHeader, totalDifficulty: Option[BigInt], pendingBlock: Boolean): BlockResponse =
    BlockResponse(
      block = Block(signedBlockHeader, BlockBody(Nil, Nil)),
      totalDifficulty = totalDifficulty,
      pendingBlock = pendingBlock
    )

}
