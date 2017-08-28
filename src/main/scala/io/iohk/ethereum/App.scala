package io.iohk.ethereum

import java.nio.file.Files

import io.iohk.ethereum.blockchain.sync.SyncController
import io.iohk.ethereum.db.dataSource.{EphemDataSource, LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage.{ArchiveNodeStorage, NodeStorage}
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.discovery.DiscoveryListener
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.nodebuilder.Node

import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object App {

  val NumberOfInsertions = 100000
  val NumberOfInsertionsPerBatch = 10000
  val NumberOfRounds = 2

  def main(args: Array[String]): Unit = {

    var roundsMeasured = Seq.fill(NumberOfInsertions / NumberOfInsertionsPerBatch)(0: Double)

    (1 to NumberOfRounds).foreach { i =>
      val timesMeasured = measureRound()
      roundsMeasured = roundsMeasured.zip(timesMeasured).map{ case (x, y) => x + y}
      println(s"Round $i measured")
    }

    roundsMeasured.zipWithIndex.foreach { case (totalAmount, index) =>
      val i = (index + 1) * NumberOfInsertionsPerBatch
      val delta = totalAmount / NumberOfRounds
      println(s"=== $i elements put, time for batch is: $delta sec")
    }

  }

  def measureRound(): Seq[Double] = {
    var res = Seq.empty[Double]

    val ephemDataSource = EphemDataSource()

    val ns = new ArchiveNodeStorage(new NodeStorage(ephemDataSource))

    val hashFn = crypto.kec256(_: Array[Byte])

    val defaultByteArraySer = MerklePatriciaTrie.defaultByteArraySerializable
    val EmptyTrie = MerklePatriciaTrie[Array[Byte], Array[Byte]](ns)(defaultByteArraySer, defaultByteArraySer)

    var t = System.currentTimeMillis()
    (1 to NumberOfInsertions).foldLeft(EmptyTrie){case (trie, i) =>
      val k = hashFn(("hello" + i).getBytes)
      val v = hashFn(("world" + i).getBytes)

      if (i % NumberOfInsertionsPerBatch == 0) {
        val newT = System.currentTimeMillis()
        val delta = (newT - t) / 1000.0
        res = res :+ delta
        t = newT
      }
      trie.put(k, v)
    }

    ephemDataSource.clear

    res
  }
}
