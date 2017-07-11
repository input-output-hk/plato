package io.iohk.ethereum.mpt

import java.nio.ByteBuffer
import java.nio.file.Files

import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.NodeStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie.defaultByteArraySerializable

/**
  * Created by alan on 14/06/17.
  */
object MeetupShow {

  val hashFn = kec256(_: Array[Byte])

  val dbPath = Files.createTempDirectory("testdb").toAbsolutePath.toString
  val dataSource = LevelDBDataSource(new LevelDbConfig {
    override val cacheSize: Int = 0
    override val verifyChecksums: Boolean = true
    override val paranoidChecks: Boolean = true
    override val createIfMissing: Boolean = true
    override val path: String = dbPath
  })

  val key = "aaaaa"

  val keyValue1 = "1"
  val keyValue2 = "2"

  val storage = new NodeStorage(dataSource)

  def main(args: Array[String]): Unit = {

    val emptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](storage, hashFn)

    var newMpt = emptyTrie.put(key.getBytes(), keyValue1.getBytes())
    newMpt = newMpt.put("aa".getBytes(), "othervalue".getBytes())

    val stateRootHashAtBlockN = newMpt.getRootHash

    val reconstructed =
      MerklePatriciaTrie[Array[Byte], Array[Byte]](stateRootHashAtBlockN, storage, hashFn)

    val retrieve: Option[String] = reconstructed.get(key.getBytes()).map(new String(_))
    val retrieve2: Option[String] = reconstructed.get("aa".getBytes()).map(new String(_))
    println(s"Should show set value for key1 $retrieve")
    println(s"Should show set value for other $retrieve2")

    val alteredStateTrie = reconstructed.put(key.getBytes(), keyValue2.getBytes())

    println("Should show new value for key1 " + alteredStateTrie.get(key.getBytes()).map(new String(_)))
    val stateRootHashAtBlockNPlus = alteredStateTrie.getRootHash

    // Only the nodes that change get added to the new MPT
    val stateAtBlockN = MerklePatriciaTrie[Array[Byte], Array[Byte]](stateRootHashAtBlockN, storage, hashFn)
    val stateAtBlockNPlus = MerklePatriciaTrie[Array[Byte], Array[Byte]](stateRootHashAtBlockNPlus, storage, hashFn)

    println("Should show value for key1 at Block N " + stateAtBlockN.get(key.getBytes()).map(new String(_)))
    println("Should show value for key1 at Block N Plus " + stateAtBlockNPlus.get(key.getBytes()).map(new String(_)))


  }
}
