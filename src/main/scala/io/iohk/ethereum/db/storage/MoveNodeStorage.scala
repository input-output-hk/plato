package io.iohk.ethereum.db.storage

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.NodeStorage._

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  *   Key: hash of the RLP encoded node
  *   Value: the RLP encoded node
  */
class MoveNodeStorage(val dataSource: DataSource) extends KeyValueStorage[NodeHash, NodeEncoded, MoveNodeStorage] {

  val namespace: IndexedSeq[Byte] = Namespaces.NodeMoveNamespace
  def keySerializer: NodeHash => IndexedSeq[Byte] = _.toIndexedSeq
  def valueSerializer: NodeEncoded => IndexedSeq[Byte] = _.toIndexedSeq
  def valueDeserializer: IndexedSeq[Byte] => NodeEncoded = _.toArray

  protected def apply(dataSource: DataSource): MoveNodeStorage = new MoveNodeStorage(dataSource)
}


