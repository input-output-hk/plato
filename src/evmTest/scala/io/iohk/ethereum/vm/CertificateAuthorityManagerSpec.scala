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
      constructorArgs = Seq(addressA, atLeastOneVote)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 3).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 5).call().returnData shouldBe UInt256(true).bytes
  }

  "CertificateAuthorityManager" should "accept the new proposed CA and add it to the round robin" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, atLeastOneVote)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressB).call(sender = addressA)
    /**
      * @note Currently when a new CA is added, the next block repeat the previous CA elected
      */
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 3).call().returnData shouldBe UInt256(true).bytes
  }

  "CertificateAuthorityManager" should "NOT accept the new proposed CA if the sender is not part of the CA's list" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, atLeastOneVote)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressB).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 3).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 3).call().returnData shouldBe UInt256(false).bytes
  }

  "CertificateAuthorityManager" should "remove the proposed CA and remove it from the round robin" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, atLeastOneVote)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressB).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 3).call().returnData shouldBe UInt256(true).bytes
    c.voteForRemoveCA(addressA).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressB, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 4).call().returnData shouldBe UInt256(false).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 5).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 5).call().returnData shouldBe UInt256(false).bytes
  }

  "CertificateAuthorityManager" should "NOT remove the proposed CA it the sender it not part of the CA's list" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, atLeastOneVote)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForRemoveCA(addressA).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
  }

  // scalastyle:off magic.number
  "Contract" should "failure if the consensus approval percentage is not a percentage" in new TestSetup {
    val invalidPercentage = 101
    val (result, _) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, invalidPercentage)
    )
    result.error.isDefined shouldBe true
  }

  // scalastyle:off magic.number
  "The Consensus" should "be driven by their authorities majority" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, majority)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressB).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 3).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressC).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 5).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressC).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 6).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 7).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressC, 8).call().returnData shouldBe UInt256(true).bytes
    c.voteForRemoveCA(addressA).call(sender = addressC)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 9).call().returnData shouldBe UInt256(true).bytes
    c.voteForRemoveCA(addressA).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressB, 10).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressC, 11).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 12).call().returnData shouldBe UInt256(true).bytes
  }

  // scalastyle:off magic.number
  "The Consensus" should "be driven by their authorities totally agreement" in new TestSetup {
    val (_, c) = deployContract(
      "CertificateAuthorityManager",
      gasPrice = 0,
      constructorArgs = Seq(addressA, totalAgreement)
    )
    c.isElectedCertificateAuthorityForNextBlock(addressA, 1).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressB).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 2).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 3).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressC).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 4).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 5).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressC).call(sender = addressA)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 6).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 7).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressC, 8).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressD).call(sender = addressA)
    c.voteForAddCA(addressD).call(sender = addressB)
    c.isElectedCertificateAuthorityForNextBlock(addressA, 9).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressB, 10).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressC, 11).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressA, 12).call().returnData shouldBe UInt256(true).bytes
    c.voteForAddCA(addressD).call(sender = addressC)
    c.isElectedCertificateAuthorityForNextBlock(addressB, 13).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressC, 14).call().returnData shouldBe UInt256(true).bytes
    c.isElectedCertificateAuthorityForNextBlock(addressD, 15).call().returnData shouldBe UInt256(true).bytes
  }

  trait TestSetup extends EvmTestEnv {
    val addressA: Address = createAccount(0)
    val addressB: Address = createAccount(0)
    val addressC: Address = createAccount(0)
    val addressD: Address = createAccount(0)
    // consensus approval percentage
    val atLeastOneVote = 0
    val majority = 66
    val totalAgreement = 100
  }
}
