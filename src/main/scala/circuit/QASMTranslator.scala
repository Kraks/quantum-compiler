package quantum.circuit

// Translate to OpenQASM format

import scala.util.continuations._
import scala.collection.immutable.{List => SList}

import Syntax.{Exp => QExp, _}

object QASMTranslator {
  var qreg: String = "q"

  def emit(g: Gate): Unit =
    g match {
      case CCX(Bit(true), Bit(true), z) =>
        println(s"x $qreg[$z]")
      case CCX(Bit(true), y, z) =>
        println(s"cx $qreg[$y] $qreg[$z]")
      case CCX(x, y, z) =>
        println(s"ccx $qreg[$x] $qreg[$y] $qreg[$z]")
      case H(x) =>
        println(s"h $qreg[$x]")
    }
  def emit(c: Circuit): Unit = c.foreach(emit)
}
