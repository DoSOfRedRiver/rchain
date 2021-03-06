package coop.rchain.comm

import java.net.SocketTimeoutException
import scala.util.{Failure, Success, Try}
import scala.collection.concurrent
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.{Duration, MILLISECONDS}
import coop.rchain.kademlia.PeerTable

/**
  * Implements the lower levels of the network protocol.
  */
case class UnicastNetwork(id: NodeIdentifier, endpoint: Endpoint) extends ProtocolHandler {

  case class PendingKey(remote: Seq[Byte], timestamp: Long, seq: Long)

  val pending =
    new concurrent.TrieMap[PendingKey, Promise[Try[ProtocolMessage]]]

  val local = new ProtocolNode(id, endpoint, this)
  val comm = UnicastComm(local)
  val table = PeerTable(local)

  private val receiver = new Thread {
    override def run =
      while (true) {
        comm.recv match {
          case Success(res) =>
            for {
              msg <- ProtocolMessage.parse(res)
            } dispatch(msg)
          case Failure(ex: SocketTimeoutException) => ()
          case Failure(ex)                         => ex.printStackTrace
        }
      }
  }.start

  private def dispatch(msg: ProtocolMessage): Unit =
    for {
      sender <- ProtocolMessage.sender(msg)
    } {
      table.observe(new ProtocolNode(sender, this))
      msg match {
        case ping @ PingMessage(_, _)     => handlePing(sender, ping)
        case pong @ PongMessage(_, _)     => handlePong(sender, pong)
        case lookup @ LookupMessage(_, _) => handleLookup(sender, lookup)
        case lookupResponse @ LookupResponseMessage(_, _) =>
          handleLookupResponse(sender, lookupResponse)
      }
    }

  /**
    * Validate incoming PING and send responding PONG.
    */
  private def handlePing(sender: PeerNode, ping: PingMessage): Unit =
    for {
      pong <- ProtocolMessage.pong(local, ping)
    } {
      comm.send(ProtocolMessage.toBytes(pong), sender)
    }

  /**
    * Validate incoming PONG and complete its pending promise.
    */
  private def handlePong(sender: PeerNode, pong: PongMessage): Unit =
    for {
      ret <- ProtocolMessage.returnHeader(pong)
      promise <- pending.get(PendingKey(sender.key, ret.timestamp, ret.seq))
    } {
      try {
        promise.success(Success(pong))
      } catch {
        case ex: java.lang.IllegalStateException => () // Future already completed
      }
    }

  /**
    * Validate incoming LOOKUP message and return an answering
    * LOOKUP_RESPONSE.
    */
  private def handleLookup(sender: PeerNode, lookup: LookupMessage) =
    for {
      id <- ProtocolMessage.lookupId(lookup)
      resp <- ProtocolMessage.lookupResponse(local, lookup, table.lookup(id))
    } {
      comm.send(ProtocolMessage.toBytes(resp), sender)
    }

  /**
    * Validate and handle incoming LOOKUP_RESPONSE message.
    */
  private def handleLookupResponse(sender: PeerNode, response: LookupResponseMessage) =
    for {
      ret <- ProtocolMessage.returnHeader(response)
      promise <- pending.get(PendingKey(sender.key, ret.timestamp, ret.seq))
    } {
      try {
        promise.success(Success(response))
      } catch {
        case ex: java.lang.IllegalStateException => () // Future already completed
      }
    }

  /**
    * Broadcast a message to all peers in the Kademlia table.
    */
  override def broadcast(msg: ProtocolMessage): Seq[Try[Unit]] = {
    val bytes = ProtocolMessage.toBytes(msg)
    table.peers.par.map { p =>
      comm.send(bytes, p)
    }.toList
  }

  /**
    * Send a message to a single, identified peer and return any
    * response that arrives within `timeout`. Receipt of messages is
    * asynchronous in this protocol, so create a promise and await its
    * completion.
    *
    * This method should be called in its own thread.
    */
  override def roundTrip(msg: ProtocolMessage,
                         remote: ProtocolNode,
                         timeout: Duration = Duration(500, MILLISECONDS)): Try[ProtocolMessage] =
    ProtocolMessage.header(msg) match {
      case Some(header) => {
        val bytes = ProtocolMessage.toBytes(msg)
        val pend = PendingKey(remote.key, header.timestamp, header.seq)
        val promise = Promise[Try[ProtocolMessage]]
        pending.put(pend, promise)
        try {
          comm.send(bytes, remote)
          Await.result(promise.future, timeout)
        } catch {
          case ex: Throwable => Failure(ex)
        } finally {
          pending.remove(pend)
          ()
        }
      }
      case None => Failure(new Exception("malformed message"))
    }

  override def toString = s"#{Network $local ${local.endpoint.udpSocketAddress}}"
}
