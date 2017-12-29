package io.iohk.ethereum.vm

import io.iohk.ethereum.vm.utils.EvmTestEnv
import org.scalatest.{FlatSpec, Matchers}
import io.iohk.ethereum.domain.{Address, UInt256}

// TODO: Improve assertions in order to make them more readable
class CertificateAuthorityManagerSpec extends FlatSpec with Matchers {

  "CertificateAuthorityManager" should "elect the CA for slot number 1,2,3,4,5" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(ca, consensusApprovalPercentage)
    )
    c.isElectedCertificateAuthorityForNextBlock(ca, 1).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 3).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 5).call().returnData shouldBe UInt256(true).bytes
  }

  "CertificateAuthorityManager" should "accept the new proposed CA and add it to the round robin" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(ca, consensusApprovalPercentage)
    )
    c.isElectedCertificateAuthorityForNextBlock(ca, 1).call().returnData shouldBe UInt256(true).bytes
    c.addCA(caCandidate).call(sender = ca)
    /**
      * @note Currently when a new CA is added, the next block repeat the previous CA elected
      */
    c.isElectedCertificateAuthorityForNextBlock(ca, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(caCandidate, 3).call().returnData shouldBe UInt256(true).bytes
  }

  "CertificateAuthorityManager" should "NOT accept the new proposed CA if the sender is not part of the CA's list" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(ca, consensusApprovalPercentage)
    )
    c.isElectedCertificateAuthorityForNextBlock(ca, 1).call().returnData shouldBe UInt256(true).bytes
    c.addCA(caCandidate).call(sender = caCandidate)
    c.isElectedCertificateAuthorityForNextBlock(ca, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 3).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(caCandidate, 3).call().returnData shouldBe UInt256(false).bytes
  }

  "CertificateAuthorityManager" should "remove the proposed CA and remove it from the round robin" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(ca, consensusApprovalPercentage)
    )
    c.isElectedCertificateAuthorityForNextBlock(ca, 1).call().returnData shouldBe UInt256(true).bytes
    c.addCA(caCandidate).call(sender = ca)
    c.isElectedCertificateAuthorityForNextBlock(ca, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(caCandidate, 3).call().returnData shouldBe UInt256(true).bytes
    c.removeCA(ca).call(sender = caCandidate)
    c.isElectedCertificateAuthorityForNextBlock(caCandidate, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 4).call().returnData shouldBe UInt256(false).bytes
    c.isElectedCertificateAuthorityForNextBlock(caCandidate, 5).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(ca, 5).call().returnData shouldBe UInt256(false).bytes
  }

  "CertificateAuthorityManager" should "NOT remove the proposed CA it the sender it not part of the CA's list" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(ca, consensusApprovalPercentage)
    )
    c.isElectedCertificateAuthorityForNextBlock(ca, 1).call().returnData shouldBe UInt256(true).bytes
    c.removeCA(ca).call(sender = caCandidate)
    c.isElectedCertificateAuthorityForNextBlock(ca, 2).call().returnData shouldBe UInt256(true).bytes
  }

  trait TestSetup extends EvmTestEnv {
    val ca: Address = createAccount(0)
    val caCandidate: Address = createAccount(0)
    val consensusApprovalPercentage = 0
  }
}
