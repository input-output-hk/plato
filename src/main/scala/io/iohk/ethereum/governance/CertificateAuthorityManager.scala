package io.iohk.ethereum.governance

import java.io.File

import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, OuroborosConfig}
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.vm.utils.{Contract, Utils}


trait CertificateAuthorityManager {
  def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean
}

case class CertificateAuthorityManagerImpl(
                                            blockchain: BlockchainImpl,
                                            blockchainConfig: BlockchainConfig,
                                            ouroborosConfig: OuroborosConfig
                                          ) extends CertificateAuthorityManager with Logger {

  val abis = Utils.loadContractAbiFromFile(new File(ouroborosConfig.consensusContractFilepath + ".abi")).toOption.get
  val contractBuilder: BlockHeader => Contract[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] =
    bh => {
      val world = blockchain.getReadOnlyWorldStateProxy(Some(bh.number), blockchainConfig.accountStartNonce, Some(bh.stateRoot))
      val evmConfig = EvmConfig.forBlock(bh.number, blockchainConfig)
      Contract[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage](ouroborosConfig.consensusContractAddress, bh, world, abis, evmConfig)
    }

  override def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean = {
    val block = resolveBlock()
    val contract = contractBuilder(block.signedHeader.header)
    val execResult = contract.isCertificateAuthorityFor(address, slotNumber).call()
    execResult.returnData.toArray.last == 1.toByte
  }

  // TODO: Improve the case of an empty block
  private def resolveBlock(): Block = {
    blockchain.getBlockByNumber(blockchain.getBestBlockNumber()).get
  }
}
