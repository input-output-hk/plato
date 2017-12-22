package io.iohk.ethereum.governance

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain._
import io.iohk.ethereum.jsonrpc.EthService.{CallTx, ResolvedBlock}
import io.iohk.ethereum.jsonrpc.{JsonRpcError, JsonRpcErrors}
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex


trait CertificateAuthorityManager {
  def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean
}

case class CertificateAuthorityManagerImpl(
    simulateTx: (SignedTransaction, BlockHeader) => TxResult,
    blockchain: Blockchain
) extends CertificateAuthorityManager with Logger {

  override def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean = {
    val functionSelector = "f74f1978" // NOTE: web3.sha3("isCertificateAuthorityFor(address, uint256)")
    val certificateAuthorityAddress = "0000000000000000000000000000000000000000000000000000000000000000"
    val slotNumber = "0000000000000000000000000000000000000000000000000000000000000000"
    val data = functionSelector + certificateAuthorityAddress + slotNumber

    val tx = CallTx(
      Some(ByteString(Hex.decode(address.toUnprefixedString))), // TODO: Miner address
      Some(ByteString(Hex.decode("000000000000000000000000000000000000000a"))), // TODO: Contract address
      Some(0), // TODO: Check if this is a correct value for gasLimit
      0,
      0,
      ByteString(Hex.decode(data)))

    val txResult = for {
      stx   <- prepareTransaction(tx)
      block <- resolveBlock()
    } yield simulateTx(stx, block.block.signedHeader.header)

    val result = txResult.map(_.vmReturnData.toString().last == '1').right.get
    log.debug("********* vmReturnData = ", result)
    result
  }

  private def prepareTransaction(callTx: CallTx): Either[JsonRpcError ,SignedTransaction] = {
    val fromAddress = callTx.from.map(Address.apply).get
    val toAddress = callTx.to.map(Address.apply)
    val gasLimit = callTx.gas.get
    val tx = Transaction(0, callTx.gasPrice, gasLimit, toAddress, callTx.value, callTx.data)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    Right(SignedTransaction(tx, fakeSignature, fromAddress))
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
