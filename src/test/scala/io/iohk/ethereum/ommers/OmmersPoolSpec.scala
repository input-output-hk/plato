/* TODO: Remove it when ommer functionality dissapear
package io.iohk.ethereum.ommers

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.ByteString
import io.iohk.ethereum.Fixtures.Blocks.Block3125369
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, GetOmmers, RemoveOmmers}
import io.iohk.ethereum.utils.MiningConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class OmmersPoolSpec extends FlatSpec with Matchers with MockFactory {

  "OmmersPool" should "accept ommers" in new TestSetup {
    //just return header
    (blockchain.getSignedBlockHeaderByHash _).expects(*).returns(Some(Block3125369.signedHeader))

    ommersPool ! AddOmmers(Block3125369.signedHeader)
    ommersPool.!(GetOmmers(Block3125369.signedHeader.number + 1))(testProbe.ref)

    testProbe.expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(Block3125369.signedHeader)))
  }

  "OmmersPool" should "removes ommers ommers" in new TestSetup {
    //just return header
    (blockchain.getSignedBlockHeaderByHash _).expects(*).returns(Some(Block3125369.signedHeader))

    ommersPool ! AddOmmers(Block3125369.signedHeader)
    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 2))
    ommersPool ! RemoveOmmers(Block3125369.signedHeader)

    ommersPool.!(GetOmmers(3))(testProbe.ref)

    testProbe.expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(Block3125369.signedHeader.copy(number = 2))))
  }

  "OmmersPool" should "returns ommers when out of pool siez" in new TestSetup {
    //just return header
    (blockchain.getSignedBlockHeaderByHash _).expects(*).returns(Some(Block3125369.signedHeader))

    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 4))
    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 20))
    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 30))
    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 40))
    ommersPool ! AddOmmers(Block3125369.signedHeader.copy(number = 5))
    ommersPool.!(GetOmmers(6))(testProbe.ref)

    testProbe.expectMsg(Timeouts.normalTimeout, OmmersPool.Ommers(Seq(Block3125369.signedHeader.copy(number = 5))))
  }

  trait TestSetup extends MockFactory {
    implicit val system = ActorSystem("OmmersPoolSpec_System")

    val miningConfig = new MiningConfig {
      override val ommersPoolSize: Int = 3
      override val ommerPoolQueryTimeout: FiniteDuration = Timeouts.normalTimeout
      override val blockCacheSize: Int = 4
      override val headerExtraData: ByteString = ByteString.empty
    }

    val testProbe = TestProbe()

    val blockchain = mock[BlockchainImpl]
    val ommersPool = system.actorOf(OmmersPool.props(blockchain, miningConfig))
  }
}*/
