package io.iohk.ethereum.validators

import akka.util.ByteString
import io.iohk.ethereum.domain._
import io.iohk.ethereum.pos.ElectionManager
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig}

trait BlockHeaderValidator {
  def validate(blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]): Either[BlockHeaderError, BlockHeaderValid]

  def validate(blockHeader: BlockHeader, blockchain: Blockchain): Either[BlockHeaderError, BlockHeaderValid] =
    validate(blockHeader, blockchain.getBlockHeaderByHash _)
}

object BlockHeaderValidatorImpl {
  val MaxExtraDataSize: Int = 32
  val GasLimitBoundDivisor: Int = 1024
  val MinGasLimit: BigInt = 5000 //Although the paper states this value is 125000, on the different clients 5000 is used
  val MaxGasLimit = Long.MaxValue // max gasLimit is equal 2^63-1 according to EIP106
}

class BlockHeaderValidatorImpl(blockchainConfig: BlockchainConfig,
                               electionManager: ElectionManager,
                               slotCalculator: SlotTimestampConverter) extends BlockHeaderValidator {

  import BlockHeaderValidatorImpl._
  import BlockHeaderError._

  val difficulty = new DifficultyCalculator(blockchainConfig)

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader BlockHeader to validate.
    * @param parentHeader BlockHeader of the parent of the block to validate.
    */
  def validate(blockHeader: BlockHeader, parentHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      _ <- validateExtraData(blockHeader)
      _ <- validateTimestamp(blockHeader, parentHeader)
      _ <- validateDifficulty(blockHeader, parentHeader)
      _ <- validateGasUsed(blockHeader)
      _ <- validateGasLimit(blockHeader, parentHeader)
      _ <- validateNumber(blockHeader, parentHeader)
      _ <- validateIsLeader(blockHeader)
      _ <- validateSlotNumber(blockHeader, parentHeader)
    } yield BlockHeaderValid
  }

  /** This method allows validate a BlockHeader (stated on
    * section 4.4.4 of http://paper.gavwood.com/).
    *
    * @param blockHeader BlockHeader to validate.
    * @param getBlockHeaderByHash function to obtain the parent header.
    */
  def validate(blockHeader: BlockHeader, getBlockHeaderByHash: ByteString => Option[BlockHeader]): Either[BlockHeaderError, BlockHeaderValid] = {
    for {
      blockHeaderParent <- getBlockHeaderByHash(blockHeader.parentHash).map(Right(_)).getOrElse(Left(HeaderParentNotFoundError))
      _ <- validate(blockHeader, blockHeaderParent)
    } yield BlockHeaderValid
  }

  /**
    * Validates [[io.iohk.ethereum.domain.BlockHeader.extraData]] length
    * based on validations stated in section 4.4.4 of http://paper.gavwood.com/
    *
    * @param blockHeader BlockHeader to validate.
    * @return BlockHeader if valid, an [[HeaderExtraDataError]] otherwise
    */
  private def validateExtraData(blockHeader: BlockHeader): Either[BlockHeaderError, BlockHeaderValid] = {

    def validateDaoForkExtraData(blockHeader: BlockHeader, daoForkConfig: DaoForkConfig): Either[BlockHeaderError, BlockHeaderValid] =
      (daoForkConfig requiresExtraData blockHeader.number, daoForkConfig.blockExtraData) match {
        case (false, _) =>
          Right(BlockHeaderValid)
        case (true, Some(forkExtraData)) if blockHeader.extraData == forkExtraData =>
          Right(BlockHeaderValid)
        case _ =>
          Left(DaoHeaderExtraDataError)
      }

    if (blockHeader.extraData.length <= MaxExtraDataSize) {
      import blockchainConfig._
      daoForkConfig.map(c => validateDaoForkExtraData(blockHeader, c)).getOrElse(Right(BlockHeaderValid))
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
    val coinbase = Address(blockHeader.beneficiary)
    val isLeader = electionManager.verifyIsLeader(coinbase, blockHeader.slotNumber)
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
    val isAfterParent = blockHeader.slotNumber > parentHeader.slotNumber
    val isFromFuture = System.currentTimeMillis() < slotCalculator.getSlotStartingTime(blockHeader.slotNumber)
    if (isAfterParent && !isFromFuture) Right(BlockHeaderValid)
    else Left(HeaderSlotNumberError)
  }
}

sealed trait BlockHeaderError

object BlockHeaderError {
  case object HeaderParentNotFoundError extends BlockHeaderError
  case object HeaderExtraDataError extends BlockHeaderError
  case object DaoHeaderExtraDataError extends BlockHeaderError
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
