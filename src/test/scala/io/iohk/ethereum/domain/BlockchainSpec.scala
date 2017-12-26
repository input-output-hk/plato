package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.blockchain.EphemBlockchainTestSetup
import io.iohk.ethereum.db.storage.ArchiveNodeStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import org.scalatest.{FlatSpec, Matchers}

class BlockchainSpec extends FlatSpec with Matchers {

  "Blockchain" should "be able to store a block and return if if queried by hash" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
    val block = blockchain.getBlockByHash(validBlock.signedHeader.hash)
    assert(block.isDefined)
    assert(validBlock == block.get)
    val blockHeader = blockchain.getSignedBlockHeaderByHash(validBlock.signedHeader.hash)
    assert(blockHeader.isDefined)
    assert(validBlock.signedHeader == blockHeader.get)
    val blockBody = blockchain.getBlockBodyByHash(validBlock.signedHeader.hash)
    assert(blockBody.isDefined)
    assert(validBlock.body == blockBody.get)
  }

  it should "be able to store a block and retrieve it by number" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock)
    val block = blockchain.getBlockByNumber(validBlock.signedHeader.header.number)
    assert(block.isDefined)
    assert(validBlock == block.get)
  }

  it should "be able to query a stored blockHeader by it's number" in new EphemBlockchainTestSetup {
    val validHeader = Fixtures.Blocks.ValidBlock.signedHeader
    blockchain.save(validHeader)
    val header = blockchain.getSignedBlockHeaderByNumber(validHeader.header.number)
    assert(header.isDefined)
    assert(validHeader == header.get)
  }

  it should "not return a value if not stored" in new EphemBlockchainTestSetup {
    assert(blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.signedHeader.header.number).isEmpty)
    assert(blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.signedHeader.hash).isEmpty)
  }

  it should "return an account given an address and a block number" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.signedHeader

    val emptyMpt = MerklePatriciaTrie[Address, Account](
      new ArchiveNodeStorage(storagesInstance.storages.nodeStorage)
    )

    val mptWithAcc = emptyMpt.put(address, account)
    val headerWithAcc = validHeader.header.copy(stateRoot = ByteString(mptWithAcc.getRootHash))
    val signedHeaderWithAcc = validHeader.copy(header = headerWithAcc)

    blockchain.save(signedHeaderWithAcc)

    val retrievedAccount = blockchain.getAccount(address, signedHeaderWithAcc.header.number)
    retrievedAccount shouldEqual Some(account)
  }
}
