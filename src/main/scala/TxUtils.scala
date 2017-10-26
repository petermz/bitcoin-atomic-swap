import org.bitcoinj.core._
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.bitcoinj.script.ScriptOpCodes._
import scala.concurrent.duration._

object TxUtils {
  type ByteStr = Array[Byte]
  val Params = TestNet3Params.get
  val Fee = Coin.MILLICOIN
  val Timeout = 10 seconds

  private def createLockScript(secretHash: ByteStr, until: Long, senderPublicKey: ByteStr, recipientPublicKey: ByteStr) =
    new ScriptBuilder().op(OP_DEPTH).op(OP_2).op(OP_EQUAL)
      .op(OP_IF).op(OP_SHA256).data(secretHash).op(OP_EQUALVERIFY).data(recipientPublicKey).op(OP_CHECKSIG)///use addresses instead of PKs
      .op(OP_ELSE).number(until).op(OP_CHECKLOCKTIMEVERIFY).op(OP_DROP).data(senderPublicKey).op(OP_CHECKSIG).op(OP_ENDIF).build

  private def sign(tx: Transaction, inputId: String, inputIndex: Int, sender: ECKey, outputScript: Option[Script] = None) = {
    val os = outputScript getOrElse ScriptBuilder.createOutputScript(sender.toAddress(Params))
    val input = tx.addInput(Sha256Hash.wrap(inputId), inputIndex, os)
    tx.calculateSignature(0, sender, os, SigHash.ALL, false)
  }

  def lock(inputId: String, inputIndex: Int, amount: Coin, secretHash: ByteStr, sender: ECKey, recipientPublicKey: ByteStr) = {
    val tx = new Transaction(Params)
    val until = System.currentTimeMillis / 1000 + Timeout.toSeconds
    tx.addOutput(amount.minus(Fee), createLockScript(secretHash, until, sender.getPubKey, recipientPublicKey))
    val sig = sign(tx, inputId, inputIndex, sender)
    tx.getInput(0).setScriptSig(ScriptBuilder.createInputScript(sig, sender))
    tx
  }

  def unlock(inputTx: Transaction, index: Int, amount: Coin, secret: Option[ByteStr], sender: ECKey, recipient: Address) = {
    val tx = new Transaction(Params)
    tx.addOutput(amount.minus(Fee), ScriptBuilder.createOutputScript(recipient))
    val sig = sign(tx, inputTx.getHashAsString, index, sender, Some(inputTx.getOutput(0).getScriptPubKey))
    val sb = secret.foldLeft(new ScriptBuilder().data(sig.encodeToBitcoin)) { case (builder, s) => builder.data(s) }
    tx.getInput(0).setScriptSig(sb.build)
    tx
  }
}
