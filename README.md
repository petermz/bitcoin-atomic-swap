This is an _Atomic Swap_ protocol implementation in Bitcoin blockchain using BitcoinJ and Scala.

Both parties lock their funds in a script where they can be
- claimed by the other party, provided they demonstrate they know some secret, or
- claimed back by the original party after some timeout.

To unlock the script, both parties must know some secret, which is made up by one of the parties. That party must reveal this secret in order to claim its funds, so it becomes known to the other party which, in turn, can now claim its funds. If the former party fails to reveal the secret, both parties claim their funds back after the timeout expires.
