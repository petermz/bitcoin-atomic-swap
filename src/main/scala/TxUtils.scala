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

  private def createLockScript(secretHash: ByteStr, until: Long, sender: Address, recipient: Address) =
    new ScriptBuilder().op(OP_DEPTH).op(OP_3).op(OP_EQUAL)
      .op(OP_IF).op(OP_SHA256).data(secretHash).op(OP_EQUALVERIFY).op(OP_DUP).op(OP_HASH160).data(recipient.getHash160)
      .op(OP_ELSE).number(until).op(OP_CHECKLOCKTIMEVERIFY).op(OP_DROP).op(OP_DUP).op(OP_HASH160).data(sender.getHash160)
      .op(OP_ENDIF).op(OP_EQUALVERIFY).op(OP_CHECKSIG).build

  private def sign(tx: Transaction, inputId: String, inputIndex: Int, sender: ECKey, outputScript: Option[Script] = None) = {
    val os = outputScript getOrElse ScriptBuilder.createOutputScript(sender.toAddress(Params))
    val input = tx.addInput(Sha256Hash.wrap(inputId), inputIndex, os)
    tx.calculateSignature(0, sender, os, SigHash.ALL, false)
  }

  def lock(inputId: String, inputIndex: Int, amount: Coin, secretHash: ByteStr, senderKey: ECKey, recipient: Address) = {
    val tx = new Transaction(Params)
    val until = System.currentTimeMillis / 1000 + Timeout.toSeconds
    tx.addOutput(amount.minus(Fee), createLockScript(secretHash, until, senderKey.toAddress(Params), recipient))
    val sig = sign(tx, inputId, inputIndex, senderKey)
    tx.getInput(0).setScriptSig(ScriptBuilder.createInputScript(sig, senderKey))
    tx
  }

  def unlock(inputTx: Transaction, index: Int, amount: Coin, secret: Option[ByteStr], sender: ECKey, recipient: Address) = {
    val tx = new Transaction(Params)
    tx.addOutput(amount.minus(Fee), ScriptBuilder.createOutputScript(recipient))
    val sig = sign(tx, inputTx.getHashAsString, index, sender, Some(inputTx.getOutput(0).getScriptPubKey))
    val builder = new ScriptBuilder().data(sig.encodeToBitcoin).data(sender.getPubKey)
    secret foreach builder.data
    tx.getInput(0).setScriptSig(builder.build)
    tx
  }
}
