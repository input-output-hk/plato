package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.SignedBlockHeadersStorage.SignedBlockHeaderHash
import io.iohk.ethereum.domain.{SignedBlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.SignedBlockHeaderImplicits._

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class SignedBlockHeadersStorage(val dataSource: DataSource) extends KeyValueStorage[SignedBlockHeaderHash, SignedBlockHeader, SignedBlockHeadersStorage] {

  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: (SignedBlockHeaderHash) => IndexedSeq[Byte] = identity

  override def valueSerializer: (SignedBlockHeader) => IndexedSeq[Byte] = _.toBytes

  override def valueDeserializer: (IndexedSeq[Byte]) => SignedBlockHeader = b => b.toArray.toSignedBlockHeader

  override protected def apply(dataSource: DataSource): SignedBlockHeadersStorage = new SignedBlockHeadersStorage(dataSource)

}

object SignedBlockHeadersStorage {
  type SignedBlockHeaderHash = ByteString
}
