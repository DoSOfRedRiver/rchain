package coop.rchain

import coop.rchain.roscala.ob._

package object roscala {
  val vmLiterals: Seq[Ob] = Seq(
    Fixnum(0),
    Fixnum(1),
    Fixnum(2),
    Fixnum(3),
    Fixnum(4),
    Fixnum(5),
    Fixnum(6),
    Fixnum(7),
    RblBool(true),
    RblBool(false),
    Nil,
    Niv
  )

  object CtxtRegName {
    val rslt    = 0
    val trgt    = 1
    val argvec  = 2
    val env     = 3
    val code    = 4
    val ctxt    = 5
    val self    = 6
    val selfEnv = 7
    val rcvr    = 8
    val monitor = 9
  }

  def regName(reg: Int): String =
    reg match {
      case 0 => "rslt"
      case 1 => "trgt"
      case 2 => "argvec"
      case 3 => "env"
      case 4 => "code"
      case 5 => "ctxt"
      case 6 => "self"
      case 7 => "selfEnv"
      case 8 => "rcvr"
      case 9 => "monitor"
      case _ => "unknown"
    }

  object VmLiteralName {
    val `0`   = 0
    val `1`   = 1
    val `2`   = 2
    val `3`   = 3
    val `4`   = 4
    val `5`   = 5
    val `6`   = 6
    val `7`   = 7
    val `#t`  = 8
    val `#f`  = 9
    val `NIL` = 10
    val `NIV` = 11
  }

  class OpcodePrettyPrinter {
    var debugIndent = 0

    def print(pc: Int, opcode: Opcode): String = {
      val s = ("  " * debugIndent) + pc + ": " + opcode.toString
      debugIndent = indent(debugIndent, opcode)
      s
    }

    private def indent(value: Int, opcode: Opcode): Int = {
      def ifNext(next: Boolean) = if (next) 0 else value

      opcode match {
        case OpPush | OpPushAlloc(_)  => value + 1
        case OpPop                    => value - 1
        case OpRtn(next)              => ifNext(next)
        case OpRtnArg(next, _)        => ifNext(next)
        case OpRtnReg(next, _)        => ifNext(next)
        case OpRtnTag(next, _)        => ifNext(next)
        case OpXmit(_, next, _)       => ifNext(next)
        case OpXmitArg(_, next, _, _) => ifNext(next)
        case OpXmitReg(_, next, _, _) => ifNext(next)
        case OpXmitTag(_, next, _, _) => ifNext(next)
        case _                        => value
      }
    }
  }

  def suicide(msg: String): Unit = {
    System.err.println(s"*** fatal error: $msg")
    System.exit(1)
  }
}
