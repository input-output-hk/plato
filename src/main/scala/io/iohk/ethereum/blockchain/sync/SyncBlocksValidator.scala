package io.iohk.ethereum.blockchain.sync

import akka.util.ByteString
import io.iohk.ethereum.domain.{Blockchain, SignedBlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.validators.BlockValidator.BlockValid
import io.iohk.ethereum.validators.{BlockValidator, Validators}

trait SyncBlocksValidator {

  import SyncBlocksValidator._
  import BlockBodyValidationResult._

  def blockchain: Blockchain
  def validators: Validators

  def validateBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody]): BlockBodyValidationResult = {
    var result: BlockBodyValidationResult = Valid
    (requestedHashes zip blockBodies)
      .map { case (hash, body) => (blockchain.getSignedBlockHeaderByHash(hash), body) }
      .forall {
        case (Some(header), body) =>
          val validationResult: Either[BlockValidator.BlockError, BlockValid] = validators.blockValidator.validateHeaderAndBody(header, body)
          result = validationResult.fold(_ => Invalid, _ => Valid)
          validationResult.isRight
        case _ =>
          result = DbError
          false
      }
    result
  }


  def checkHeadersChain(signedHeaders: Seq[SignedBlockHeader]): Boolean =
    if (signedHeaders.length > 1)
      signedHeaders.zip(signedHeaders.tail).forall { case (parent, child) =>
        parent.hash == child.header.parentHash && parent.header.number + 1 == child.header.number }
    else true
}

object SyncBlocksValidator {
  sealed trait BlockBodyValidationResult
  object BlockBodyValidationResult {
    case object Valid extends BlockBodyValidationResult
    case object Invalid extends BlockBodyValidationResult
    case object DbError extends BlockBodyValidationResult
  }
}
