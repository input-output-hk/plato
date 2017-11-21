package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.BlockchainConfig

/**
  * As the concept of difficulty doesn't apply to PoS, difficulty was set to be always 1, so that the
  * the total difficulty of a chain is equal to it's length, which is used for determining the best chain
  * FIXME: Remove it and it's usages
  */
class DifficultyCalculator(blockchainConfig: BlockchainConfig) {

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = 1

}
