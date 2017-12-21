package io.iohk.ethereum.governance

import io.iohk.ethereum.domain.{Address, BlockHeader, SignedTransaction}
import io.iohk.ethereum.ledger.Ledger.TxResult
import io.iohk.ethereum.utils.Logger

trait CertificateAuthorityManager {

  def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean
}

case class CertificateAuthorityManagerImpl(
    simulateTx: (SignedTransaction, BlockHeader) => TxResult)
  extends CertificateAuthorityManager with Logger {

  override def isCertificateAuthorityFor(address: Address, slotNumber: BigInt): Boolean = {
    /*
    val tx = CallTx(
      Some(ByteString(Hex.decode("c60b5deadccaa4258bc5698af683adac73f1e4a9"))), // TODO: Pass as argument
      Some(ByteString(Hex.decode("694002D4B7f8ba7E6C2C22A3eAA1AcA1e4D452dB"))), // TODO: Pass as argument
      Some(1), 2, 3, ByteString("c6888fa10000000000000000000000000000000000000000000000000000000000000006"))
    val response = ethService.call(CallRequest(tx, BlockParam.Latest))
    response.transformWith {
      case Success(maybeResponse) =>
        maybeResponse match {
          case Right(CallResponse(result)) =>
            log.debug("result: ", result)
            // TODO: Convert result into Boolean
            Future{ true }
          case Left(error) =>
            log.error("Failed to handle JSON", error)
            Future{ throw new Exception(error.toString) }
        }
      case Failure(ex) =>
        log.error("Failed to get future", ex)
        Future{ throw ex }
    }*/
    true
  }

}
