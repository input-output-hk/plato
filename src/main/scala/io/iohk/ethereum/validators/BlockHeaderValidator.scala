package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.governance.CertificateAuthorityManager
import io.iohk.ethereum.timing.Clock
import io.iohk.ethereum.utils.BlockchainConfig

trait BlockHeaderValidator {
  def validate(signedBlockHeader: SignedBlockHeader, getSignedBlockHeaderByHash: ByteString =>
    Option[SignedBlockHeader]): Either[BlockHeaderError, BlockHeaderValid]

  def validate(signedBlockHeader: SignedBlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeaderValid] =
    validate(signedBlockHeader, blockchain.getSignedBlockHeaderByHash _)
}

object BlockHeaderValidatorImpl {
  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val MaxGasLimit = Long.MaxValue // max gasLimit is equal 2^63-1 according to EIP106
}

class BlockHeaderValidatorImpl(blockchainConfig: BlockchainConfig,
                               certificateAuthorityManager: CertificateAuthorityManager,
                               slotTimeConverter: SlotTimeConverter,
                               clock: Clock) extends BlockHeaderValidator {

  import BlockHeaderValidatorImpl._
  import BlockHeaderError._

  val difficulty = new DifficultyCalculator(blockchainConfig)

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param signedBlockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    */
  def validate(signedBlockHeader: SignedBlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      _ <- validateSignedBlockHeaderSignature(signedBlockHeader)
      _ <- validateExtraData(signedBlockHeader.header)
      _ <- validateTimestamp(signedBlockHeader.header, parentHeader)
      _ <- validateDifficulty(signedBlockHeader.header, parentHeader)
      _ <- validateGasUsed(signedBlockHeader.header)
      _ <- validateGasLimit(signedBlockHeader.header, parentHeader)
      _ <- validateNumber(signedBlockHeader.header, parentHeader)
      _ <- validateIsLeader(signedBlockHeader.header)
      _ <- validateSlotNumber(signedBlockHeader.header, parentHeader)
    } yield BlockHeaderValid
  }

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param signedBlockHeader BlockHeader to validate.
    * @param getSignedBlockHeaderByHash function to obtain the parent header.
    */
  def validate(signedBlockHeader: SignedBlockHeader, getSignedBlockHeaderByHash: ByteString =>
    Option[SignedBlockHeader]): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      blockHeaderParent <- getSignedBlockHeaderByHash(signedBlockHeader.header.parentHash).map(Right(_)).getOrElse(Left(HeaderParentNotFoundError))
      _ <- validate(signedBlockHeader, blockHeaderParent.header)
    } yield BlockHeaderValid
  }

  /**
    * This method allows validate the blockHeader signature
    *
    * @param signedBlockHeader BlockHeader to validate.
    */
  def validateSignedBlockHeaderSignature(signedBlockHeader: SignedBlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val blockHeader = signedBlockHeader.header
    val signature = signedBlockHeader.signature
    (for {
      coinbase <- SignedBlockHeader.getSender(blockHeader, signature)
      if coinbase == Address(blockHeader.beneficiary)
    } yield BlockHeaderValid).toRight(SignedHeaderSignatureError)
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.extraData]] length
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderExtraDataError]] otherwise
    */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    if (blockHeader.extraData.length <= MaxExtraDataSize) {
      Right(BlockHeaderValid)
    } else {
      Left(HeaderExtraDataError)
    }
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.unixTimestamp]] is greater than the one of its parent
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderTimestampError]] otherwise
    */
  private def validateTimestamp(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.unixTimestamp > parentHeader.unixTimestamp) Right(BlockHeaderValid)
    else Left(HeaderTimestampError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.difficulty]] is correct
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderDifficultyError]] otherwise
    */
  private def validateDifficulty(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if (difficulty.calculateDifficulty(blockHeader.number, blockHeader.unixTimestamp, parentHeader) == blockHeader.difficulty) Right(BlockHeaderValid)
    else Left(HeaderDifficultyError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.gasUsed]] is not greater than [[io.iohk.ethereum.domain.BlockHeader.gasLimit]]
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderGasUsedError]] otherwise
    */
  private def validateGasUsed(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.gasUsed<=blockHeader.gasLimit) Right(BlockHeaderValid)
    else Left(HeaderGasUsedError)

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.gasLimit]] follows the restrictions based on its parent gasLimit
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * EIP106(https://github.com/ethereum/EIPs/issues/106) adds additional validation of maximum value for gasLimit.
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderGasLimitError]] otherwise
    */
  private def validateGasLimit(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {

    if (blockHeader.gasLimit > MaxGasLimit && blockHeader.number >= blockchainConfig.eip106BlockNumber)
      Left(HeaderGasLimitError)
    else {
      val gasLimitDiff = (blockHeader.gasLimit - parentHeader.gasLimit).abs
      val gasLimitDiffLimit = parentHeader.gasLimit / GasLimitBoundDivisor
      if (gasLimitDiff < gasLimitDiffLimit && blockHeader.gasLimit >= MinGasLimit)
        Right(BlockHeaderValid)
      else
        Left(HeaderGasLimitError)
    }
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.number]] is the next one after its parents number
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return BlockHeader if valid, an [[HeaderNumberError]] otherwise
    */
  private def validateNumber(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] =
    if(blockHeader.number == parentHeader.number + 1) Right(BlockHeaderValid)
    else Left(HeaderNumberError)

  /**
    * Validates that the coinbase is the leader of the slot to which the blockHeader belongs
    *
    * @param blockHeader BlockHeader to validate
    * @return a [[BlockHeaderValid]] if valid, an [[HeaderBeneficiaryError]] otherwise
    */
  private def validateIsLeader(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val blockCoinbase = Address(blockHeader.beneficiary)
    val isLeader = certificateAuthorityManager.isCertificateAuthorityFor(blockCoinbase, blockHeader.slotNumber)
    if(isLeader) Right(BlockHeaderValid)
    else Left(HeaderBeneficiaryError)
  }

  /**
    * Validates that the slot number is correct:
    *  - it's greater that it's parent slot number
    *  - it doesn't belong to a future slot
    *
    * @param blockHeader BlockHeader to validate
    * @param parentHeader BlockHeader of the parent of the block to validate.
    * @return a [[BlockHeaderValid]] if valid, an [[HeaderSlotNumberError]] otherwise
    */
  private def validateSlotNumber(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    val blockSlotBeginningTimeMillis = slotTimeConverter.getSlotStartingMillis(blockHeader.slotNumber)
    val currentTimeMillis = clock.now().toMillis

    val isAfterParent = blockHeader.slotNumber > parentHeader.slotNumber
    val isFromFuture = currentTimeMillis < blockSlotBeginningTimeMillis
    if (isAfterParent && !isFromFuture) Right(BlockHeaderValid)
    else Left(HeaderSlotNumberError)
  }
}

sealed trait BlockHeaderError

object BlockHeaderError {
  case object SignedHeaderSignatureError extends BlockHeaderError
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object HeaderTimestampError extends BlockHeaderError
  case object HeaderDifficultyError extends BlockHeaderError
  case object HeaderGasUsedError extends BlockHeaderError
  case object HeaderGasLimitError extends BlockHeaderError
  case object HeaderNumberError extends BlockHeaderError
  case object HeaderBeneficiaryError extends BlockHeaderError
  case object HeaderSlotNumberError extends BlockHeaderError
}

sealed trait BlockHeaderValid
case object BlockHeaderValid extends BlockHeaderValid
