package io.iohk.ethereum.governance

import java.io.File
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.ResolvedBlock
import io.iohk.ethereum.jsonrpc.{JsonRpcError, JsonRpcErrors}
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
    val block = resolveBlock().toOption.get
    val contract = contractBuilder(block.block.signedHeader.header)
    val execResult = contract.isCertificateAuthorityFor(address, slotNumber).call()
    execResult.returnData.toArray.last == 1.toByte
  }

  private def resolveBlock(): Either[JsonRpcError, ResolvedBlock] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
      blockchain.getBlockByNumber(number)
        .map(Right.apply)
        .getOrElse(Left(JsonRpcErrors.InvalidParams(s"Block $number not found")))
    }
    getBlock(blockchain.getBestBlockNumber()).map(ResolvedBlock(_, pending = false))
  }
}
