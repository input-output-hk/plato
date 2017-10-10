package io.iohk.ethereum.db

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import org.scalacheck.Gen
import org.scalatest.FlatSpec

class LevelDBDataSourceIntegrationSuite extends FlatSpec with DataSourceIntegrationTestBehavior {

  private def createDataSource(dataSourcePath: String) = LevelDBDataSource(new LevelDbConfig {
    override val verifyChecksums: Boolean = true
    override val paranoidChecks: Boolean = true
    override val createIfMissing: Boolean = true
    override val path: String = dataSourcePath
  })

  def modifiedKey(key: ByteString): ByteString = (key.head + 1).toByte +: key.tail

//  it should behave like dataSource(createDataSource)

  it should "be able to insert keys in separate updates" in {
    forAll(Gen.listOfN(100000, byteStringOfLengthNGen(KeySizeWithoutPrefix))) { keyList: Seq[ByteString] =>
      withDir { path =>
        val toUpsert = keyList.zip(keyList)

        //Do multiple updates
        val db = createDataSource(path).update(OtherNamespace, Seq(), toUpsert)
//        keyList.foreach { key => assert(db.get(OtherNamespace, key).contains(key)) }

        //Do multiple updates (changing the values)
//        val db2 = toUpsert.foldLeft(db) { case (recDB, (key, _)) =>
//          val dbAfterUpdate = recDB.update(OtherNamespace, Seq(), Seq(key -> modifiedKey(key)))
//          if(dbAfterUpdate.get(OtherNamespace, key).contains(key)) {
//            println("Key insertion not done immediately")
//            throw new Exception("Everything not OK")
//          } //else
////            println("Everything OK")
//          dbAfterUpdate
//        }

        val modifiedKeyPairList = keyList.take(100).map(k => k->modifiedKey(k))
        val modifiedKeyPairList2 = keyList.drop(100).take(100).map(k => k->modifiedKey(k))
        val modifiedKeyPairList3 = keyList.drop(200).take(100).map(k => k->modifiedKey(k))
        val modifiedKeyPairList4 = keyList.drop(300).take(100).map(k => k->modifiedKey(k))
        val dbAfterUpdate = db.update(OtherNamespace, Seq(), modifiedKeyPairList)
        val dbAfterUpdate2 = db.update(OtherNamespace, Seq(), modifiedKeyPairList2)
        val dbAfterUpdate3 = db.update(OtherNamespace, Seq(), modifiedKeyPairList3)

        if(dbAfterUpdate3.get(OtherNamespace, modifiedKeyPairList.last._1).contains(modifiedKeyPairList.last._1)) {
          println("Key insertion not done immediately")
          throw new Exception("Everything not OK")
        }

        db.destroy()
      }
    }
  }
}
