package io.iohk.ethereum.governance

import java.io.File

import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, OuroborosConfig}
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.vm.utils.{Contract, Utils}


trait CertificateAuthorityManager {
  def isElectedCertificateAuthorityFor(address: Address, parentHeader: BlockHeader, slotNumber: BigInt): Boolean
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

  override def isElectedCertificateAuthorityFor(address: Address, parentHeader: BlockHeader, slotNumber: BigInt): Boolean = {
    val contract = contractBuilder(parentHeader)
    val execResult = contract.isElectedCertificateAuthorityForNextBlock(address, slotNumber).call()
    val isElectedCA = execResult.returnData.toArray.last == 1.toByte
    if (isElectedCA) log.debug(s"Address ${address.toString} is elected as CA for slotNumber $slotNumber")
    isElectedCA
  }
}
