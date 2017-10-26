import TxUtils._
import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props}
import com.typesafe.scalalogging.StrictLogging
import org.bitcoinj.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object AtomicSwap extends App with StrictLogging {

  case class StartSwap(other: ActorRef)
  case class Handshake(pk: ByteStr)
  case class Locked(tx: Transaction, pk: ByteStr, secretHash: ByteStr)
  case class Unlocked(secret: ByteStr)
  case class Failed(tx: Transaction)

  abstract class ActorBase(sk: String) extends Actor {
    protected val key = ECKey.fromPrivate(sk.grouped(2).map(java.lang.Byte.parseByte(_, 16)).toArray)
    lazy val address = key.toAddress(Params)
    lazy val publicKey = key.getPubKey

    private var revertAction: Cancellable = _
    protected def scheduleRevert(revertTx: Transaction) =
      revertAction = context.system.scheduler.scheduleOnce(Timeout) {
        self ! Failed(revertTx)
      }
    protected def cancelRevert() = revertAction.cancel()

    private def format(msg: String) = s"${self.path.name}: $msg"
    private def format(tx: Transaction) = tx.unsafeBitcoinSerialize map { "%02X" format _ } mkString

    def log(msg: String) = logger.info(format(msg))
    def log(msg: String, tx: Transaction) = logger.info(format(msg) + " = " + format(tx))
  }

  /** Alice can either cooperate or cheat by not revealing secret to counterparty */
  class Alice(cooperative: Boolean = true) extends ActorBase("0101010001010101010101010100010101010101010101000101010101010101") {
    private val secret = "Never trust Alice".getBytes
    private var revertAction: Cancellable = _

    def receive = {
      case Handshake(pk) =>
        val amount = Coin.parseCoin("0.648")
        val secretHash = Sha256Hash.hash(secret)
        val lockTx = lock("bf5701c595e96d49d9d671ed370e218eb2bc0d4225edd49dd94b6d12e2748332", 0, amount, secretHash, key, pk)
        log("lock tx", lockTx)
        sender ! Locked(lockTx, publicKey, secretHash)
        scheduleRevert(unlock(lockTx, 0, lockTx.getOutputSum, None, key, address))

      case Locked(tx, _, _) =>
        if (cooperative) {
          val unlockTx = unlock(tx, 0, tx.getOutputSum, Some(secret), key, address)
          log("unlock tx", unlockTx)
          sender ! Unlocked(secret)
          cancelRevert()
          context.stop(self)
        } else {
          log("doesn't send secret to " + sender.path.name)
        }

      case Failed(tx) =>
        log("revert tx", tx)
        context.stop(self)
    }
  }

  /** Bob is always cooperative */
  class Bob extends ActorBase("0101000101010101010101010001010101010101010100010101010101010101") {
    private var txToUnlock: Transaction = _

    def receive = {
      case StartSwap(other) =>
        log("starts swap with " + other.path.name)
        other ! Handshake(publicKey)

      case Locked(tx, pk, secretHash) =>
        txToUnlock = tx
        val amount = Coin.parseCoin("1.284")
        val lockTx = lock("76efd4fb3f6cda2e0e3974096c12665d0009d8b0465ef4c924c37f3ee32c8e52", 0, amount, secretHash, key, pk)
        log("lock tx", lockTx)
        sender ! Locked(lockTx, publicKey, secretHash)
        scheduleRevert(unlock(lockTx, 0, lockTx.getOutputSum, None, key, address))

      case Unlocked(secret) =>
        val unlockTx = unlock(txToUnlock, 0, txToUnlock.getOutputSum, Some(secret), key, address)
        log("unlock tx", unlockTx)
        cancelRevert()
        context.stop(self)

      case Failed(tx) =>
        log("revert tx", tx)
        context.stop(self)
    }
  }

  val system = ActorSystem("BitcoinAtomicSwap")

  logger.info("cooperative case: both txs go through")
  val alice = system.actorOf(Props(new Alice), name="Alice")
  val bob = system.actorOf(Props[Bob], name="Bob")
  bob ! StartSwap(alice)
  Thread.sleep(3.seconds.toMillis)

  logger.info("uncooperative case: both txs are reverted")
  val charlie = system.actorOf(Props[Bob], name="Charlie")
  val fawkes = system.actorOf(Props(new Alice(cooperative=false)), name="Fawkes")
  charlie ! StartSwap(fawkes)
  Thread.sleep((Timeout + 3.seconds).toMillis)

  system.terminate()
}
