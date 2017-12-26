package io.iohk.ethereum.vm.utils

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.vm._
import scala.language.dynamics

case class Contract[W <: WorldStateProxy[W, S], S <: Storage[S]](address: Address, blockHeader: BlockHeader,
                                                                 world: W, abis: Seq[ABI],
                                                                 evmConfig: EvmConfig) extends Dynamic {

  def applyDynamic(methodName: String)(args: Any*): ContractMethodCall[W, S] = {
    callMethod(methodName)(args: _*)
  }

  def callMethod(methodName: String)(args: Any*): ContractMethodCall[W, S] = {
    if (methodName.contains("("))
      callWithSignature(methodName)(args: _*)
    else
      callWithMethodName(methodName)(args: _*)
  }

  private def callWithMethodName(methodName: String)(args: Any*): ContractMethodCall[W, S] = {
    val matchedAbis = abis
      .filter { a => a.inputs.size == args.size && a.name == methodName && a.`type` == "function" }

    if (matchedAbis.isEmpty)
      throw new RuntimeException("No matching ABI found. Please specify the signature")
    else if (matchedAbis.size > 1)
      throw new RuntimeException("More than one matching ABI found. Please specify the signature")
    else {
      val abi = matchedAbis.head
      callWithSignature(s"$methodName(${abi.inputs.map(_.`type`).mkString(",")})")(args: _*)
    }
  }

  private def callWithSignature(signature: String)(args: Any*): ContractMethodCall[W, S] = {
    val signatureHash = ByteString(crypto.kec256(signature.getBytes)).take(4)
    val callData = args.map(Contract.parseArg).foldLeft(signatureHash)(_ ++ _)
    ContractMethodCall(this, callData, blockHeader, evmConfig)
  }

}

object Contract {
  private[utils] def parseArg(arg: Any): ByteString = arg match {
    case b: ByteString => UInt256(b).bytes
    case b: BigInt => UInt256(b).bytes
    case a: Array[Byte] => UInt256(a).bytes
    case i: Int => UInt256(i).bytes
    case b: Byte => UInt256(b).bytes
    case a: Address => UInt256(a.bytes).bytes
    case other => throw new RuntimeException("Invalid call argument")
  }
}

case class ContractMethodCall[W <: WorldStateProxy[W, S], S <: Storage[S]](contract: Contract[W, S],
                                                                           callData: ByteString,
                                                                           blockHeader: BlockHeader,
                                                                           evmConfig: EvmConfig) {
  def call(value: BigInt = 0,
           gasLimit: BigInt = BigInt(2).pow(256) - 1,
           gasPrice: BigInt = 0,
           sender: Address = contract.address): ProgramResult[W, S] = {
    val fakeTx = Transaction(0, gasPrice, gasLimit, contract.address, value, callData)
    val fakeSignature = ECDSASignature(0, 0, 0.toByte)
    val fakeStx = SignedTransaction(fakeTx, fakeSignature, sender)

    val pc = ProgramContext[W, S](
      fakeStx, contract.address, Program(contract.world.getCode(contract.address)),
      blockHeader, contract.world, evmConfig
    )

    VM.run(pc)
  }
}
