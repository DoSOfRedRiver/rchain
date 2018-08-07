package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob.{Ctxt, Nil, Niv, Ob}

class LockedMbox extends Ob {
  override def receiveMsg(client: MboxOb, task: Ctxt, state: State): Ob = synchronized {
    MboxOb.logger.debug("Locked mailbox receives message")

    val newMbox = QueueMbox(Nil)
    newMbox.enqueue(task)
    client.mbox = newMbox
    Niv
  }

  /**
    * `nextMsg` is called after an `Actor` finished updating its
    * state. It gets called when an `Actor` is ready to process more
    * messages.
    *
    * In the case of a `LockedMbox` the only message in the mailbox
    * already got processed.
    * Therefore all that needs to be done is to unlock the given
    * mailbox.
    */
  override def nextMsg(client: MboxOb, newEnabledSet: Ob, state: State): Ob = {
    MboxOb.logger.debug(s"Unlock ${client}'s mailbox")

    if (newEnabledSet == Nil)
      client.mbox = MboxOb.EmptyMbox
    else {
      val newMbox = QueueMbox(newEnabledSet)
      newMbox.unlock()
      client.mbox = newMbox
    }

    Niv
  }
}
