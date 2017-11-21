package io.iohk.ethereum.network.p2p

import java.net.{InetSocketAddress, URI}
import java.security.SecureRandom

import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.utils.BlockchainConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import akka.actor.{ActorSystem, PoisonPill, Props, Terminated}
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.crypto.generateKeyPair
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.{Timeouts, crypto}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.network.{ForkResolver, PeerActor, PeerEventBusActor}
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status.StatusEnc
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello.HelloEnc
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong.PongEnc
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.nodebuilder.SecureRandomBuilder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import io.iohk.ethereum.network._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.Fixtures.FakeSignature
import io.iohk.ethereum.network.p2p.messages.PV62.GetSignedBlockHeaders.GetSignedBlockHeadersEnc

class PeerActorSpec extends FlatSpec with Matchers {

  val remoteNodeKey: AsymmetricCipherKeyPair = generateKeyPair(new SecureRandom)
  val remoteNodeId: ByteString = ByteString(remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId)

  val blockchainConfig = BlockchainConfig(Config.config)

  "PeerActor" should "create rlpx connection and send hello message" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: HelloEnc) => ()
    }
  }

  it should "retry failed rlpx connection" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.watch(peer)

    (0 to 3) foreach { _ =>
      time.advance(5.seconds)
      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionFailed)
    }

    rlpxConnection.expectMsgClass(classOf[Terminated])
  }

  it should "try to reconnect on broken rlpx connection" in new NodeStatusSetup with HandshakerSetup {
    implicit val system = ActorSystem("PeerActorSpec_System")

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerEventBusActor.props)
    var rlpxConnection = TestProbe() // var as we actually need new instances
    val knownNodesManager = TestProbe()

    val peer = TestActorRef(Props(new PeerActor(new InetSocketAddress("127.0.0.1", 0), _ => {
        rlpxConnection = TestProbe()
        rlpxConnection.ref
      }, peerConf, peerMessageBus, knownNodesManager.ref, false, Some(time.scheduler),
      handshaker)))

    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    rlpxConnection.expectMsgPF() {
      case RLPxConnectionHandler.SendMessage(hello: HelloEnc) => ()
    }

    rlpxConnection.ref ! PoisonPill
    peer.unwatch(rlpxConnection.ref)
    time.advance(2.seconds)
    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
  }

  it should "successfully connect to a responsive peer" in new TestSetup {
    val uri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@localhost:9000")
    val completeUri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@127.0.0.1:9000")
    peer ! PeerActor.ConnectTo(uri)
    peer ! PeerActor.ConnectTo(uri)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    //Hello exchange
    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Versions.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 0,
      totalDifficulty = daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    //Check that peer is connected
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))

    knownNodesManager.expectMsg(KnownNodesManager.AddKnownNode(completeUri))
    knownNodesManager.expectNoMsg()
  }

  it should "successfully connect to a IPv6 peer" in new TestSetup {
    val uri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@[::]:9000")
    val completeUri = new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray[Byte])}@[0:0:0:0:0:0:0:0]:9000")
    peer ! PeerActor.ConnectTo(uri)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    //Hello exchange
    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Versions.PV63.toByte)), 9000, ByteString("unused"))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 0,
      totalDifficulty = daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    //Check that peer is connected
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(Ping()))
    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Pong()))

    knownNodesManager.expectMsg(KnownNodesManager.AddKnownNode(completeUri))
    knownNodesManager.expectNoMsg()
  }

  it should "disconnect on Hello timeout" in new TestSetup {
    val connection = TestProbe()

    peer ! PeerActor.HandleConnection(connection.ref, new InetSocketAddress("localhost", 9000))

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.HandleConnection])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    time.advance(5.seconds)
    rlpxConnection.expectMsg(Timeouts.normalTimeout, RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage)))

  }

  it should "stash disconnect message until handshaked" in new TestSetup {
    peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))
    peer ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)

    rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
    rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

    val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Versions.PV63.toByte)), 9000, ByteString("unused"))

    //Hello exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

    val remoteStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 0,
      totalDifficulty = daoForkBlockTotalDifficulty + 100000, // remote is after the fork
      bestHash = ByteString("blockhash"),
      genesisHash = genesisHash)

    //Node status exchange
    rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
    rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

    rlpxConnection.expectMsg(RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TooManyPeers)))
  }

  trait BlockUtils {

    val blockBody = new BlockBody(Seq(), Seq())

    val etcForkBlockHeader = SignedBlockHeader(BlockHeader(
      parentHash = ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ommersHash = ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347")),
      beneficiary = ByteString(Hex.decode("61c808d82a3ac53231750dadc13c777b59310bd9")),
      stateRoot = ByteString(Hex.decode("614d7d358b03cbdaf0343529673be20ad45809d02487f023e047efdce9da8aff")),
      transactionsRoot = ByteString(Hex.decode("d33068a7f21bff5018a00ca08a3566a06be4196dfe9e39f96e431565a619d455")),
      receiptsRoot = ByteString(Hex.decode("7bda9aa65977800376129148cbfe89d35a016dd51c95d6e6dc1e76307d315468")),
      logsBloom = ByteString(Hex.decode("00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")),
      difficulty = 1,
      number = BigInt(1920000),
      gasLimit = BigInt(4712384),
      gasUsed = BigInt(84000),
      unixTimestamp = 1469020839L,
      extraData = ByteString(Hex.decode("e4b883e5bda9e7a59ee4bb99e9b1bc")),
      mixHash = ByteString(Hex.decode("c52daa7054babe515b17ee98540c0889cf5e1595c5dd77496997ca84a68c8da1")),
      nonce = ByteString(Hex.decode("05276a600980199d")),
      slotNumber = BigInt(1920000)),
      FakeSignature)

    val nonEtcForkBlockHeader = SignedBlockHeader(BlockHeader(
      parentHash = ByteString("this"),
      ommersHash = ByteString("is"),
      beneficiary = ByteString("not"),
      stateRoot = ByteString("an"),
      transactionsRoot = ByteString("ETC"),
      receiptsRoot = ByteString("fork"),
      logsBloom = ByteString("block"),
      difficulty = 1,
      number = BigInt(1920000),
      gasLimit = BigInt(4712384),
      gasUsed = BigInt(84000),
      unixTimestamp = 1469020839L,
      extraData = ByteString("unused"),
      mixHash = ByteString("unused"),
      nonce = ByteString("unused"),
      slotNumber = BigInt(1920000)),
      FakeSignature)
  }

  trait NodeStatusSetup extends SecureRandomBuilder with EphemBlockchainTestSetup {
    val nodeKey = crypto.generateKeyPair(secureRandom)

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening,
      discoveryStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

    val testGenesisHeader = SignedBlockHeader(BlockHeader(
      parentHash = ByteString("0"),
      ommersHash = ByteString("0"),
      beneficiary = ByteString("0"),
      stateRoot = ByteString("0"),
      transactionsRoot = ByteString("0"),
      receiptsRoot = ByteString("0"),
      logsBloom = ByteString("0"),
      difficulty = 1,
      number = 0,
      gasLimit = 4000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = ByteString("0"),
      mixHash = ByteString("0"),
      nonce = ByteString("0"),
      slotNumber = 0), FakeSignature)
    blockchain.save(testGenesisHeader)

    val daoForkBlockNumber = 1920000

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val rlpxConfiguration: RLPxConfiguration = new RLPxConfiguration {
        override val waitForTcpAckTimeout: FiniteDuration = Timeouts.normalTimeout
        override val waitForHandshakeTimeout: FiniteDuration = Timeouts.normalTimeout
      }
      override val waitForHelloTimeout: FiniteDuration = 3 seconds
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 5 seconds
      override val maxOutgoingPeers = 10
      override val maxIncomingPeers = 5
      override val maxPendingPeers = 5
      override val networkId: Int = 1

      override val updateNodesInitialDelay: FiniteDuration = 5.seconds
      override val updateNodesInterval: FiniteDuration = 20.seconds
    }

  }

  trait HandshakerSetup extends NodeStatusSetup {
    val handshakerConfiguration = new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = Some(new ForkResolver.EtcForkResolver(blockchainConfig.daoForkConfig.get))
      override val nodeStatusHolder: Agent[NodeStatus] = HandshakerSetup.this.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = HandshakerSetup.this.peerConf
      override val blockchain: Blockchain = HandshakerSetup.this.blockchain
      override val appStateStorage: AppStateStorage = HandshakerSetup.this.storagesInstance.storages.appStateStorage
    }

    val handshaker = EtcHandshaker(handshakerConfiguration)
  }

  trait TestSetup extends NodeStatusSetup with BlockUtils with HandshakerSetup {

    val genesisHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))

    val daoForkBlockTotalDifficulty: BigInt = BigInt("39490964433395682584")

    def setupConnection(): Unit = {
      peer ! PeerActor.ConnectTo(new URI("encode://localhost:9000"))

      rlpxConnection.expectMsgClass(classOf[RLPxConnectionHandler.ConnectTo])
      rlpxConnection.reply(RLPxConnectionHandler.ConnectionEstablished(remoteNodeId))

      val remoteHello = Hello(4, "test-client", Seq(Capability("eth", Versions.PV63.toByte)), 9000, ByteString("unused"))
      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: HelloEnc) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteHello))

      val remoteStatus = Status(
        protocolVersion = Versions.PV63,
        networkId = 0,
        totalDifficulty = daoForkBlockTotalDifficulty + 100000, // remote is after the fork
        bestHash = ByteString("blockhash"),
        genesisHash = genesisHash)

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: StatusEnc) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(remoteStatus))

      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetSignedBlockHeadersEnc) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(SignedBlockHeaders(Seq(etcForkBlockHeader))))

      // ask for highest block
      rlpxConnection.expectMsgPF() { case RLPxConnectionHandler.SendMessage(_: GetSignedBlockHeadersEnc) => () }
      rlpxConnection.send(peer, RLPxConnectionHandler.MessageReceived(SignedBlockHeaders(Nil)))
    }

    implicit val system = ActorSystem("PeerActorSpec_System")

    val rlpxConnection = TestProbe()

    val time = new VirtualTime

    val peerMessageBus = system.actorOf(PeerEventBusActor.props)

    val knownNodesManager = TestProbe()

    val peer = TestActorRef(Props(new PeerActor(
      new InetSocketAddress("127.0.0.1", 0),
      _ => rlpxConnection.ref,
      peerConf,
      peerMessageBus,
      knownNodesManager.ref,
      false,
      Some(time.scheduler),
      handshaker)))
  }

}
