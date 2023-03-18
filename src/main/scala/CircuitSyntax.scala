package quantum

object Syntax {
  abstract class Exp
  case class Wire(pos: Int) extends Exp {
    override def toString = pos.toString
  }
  case class Bit(b: Boolean) extends Exp {
    override def toString = s"""$$${if (b) "1" else "0"}"""
  }

  abstract class Gate
  // The Toffoli/Controlled-Controlled-Not gate
  case class CCX(x: Exp, y: Exp, z: Exp) extends Gate {
    override def toString =
      if (x == Bit(true))
        if (y == Bit(true)) s"X $z" else s"CX $y $z"
      else s"CCX $x $y $z"
  }
  // The Hadamard gate
  case class H(x: Exp) extends Gate {
    override def toString = s"H $x"
  }
  // The Not gate
  def X(x: Exp): Gate = CCX(Bit(true), Bit(true), x)
  // The Controlled-Not gate
  def CX(y: Exp, z: Exp): Gate = CCX(Bit(true), y, z)

  implicit def intToExp(i: Int): Exp = Wire(i)
  implicit def intToBool(i: Int): Boolean = i != 0

  type Circuit = List[Gate]
}

import Syntax._

object RandCircuit {
  def randGate(size: Int): Gate = ???
}

object Examples {
  // 2 qubits
  val circuit1: Circuit = List(
    H(0),
    CX(0, 1)
  )

  // 1 qubits
  val circuit2: Circuit = List(
    H(0),
    X(0),
    H(0)
  )

  // 4 qubits
  val simon: Circuit = List(
    H(0),
    H(1),
    CX(0, 2),
    CX(0, 3),
    CX(1, 2),
    CX(1, 3),
    H(0),
    H(1)
  )

  val rand4: Circuit = List(
    H(0), H(1),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3),
    H(0), H(1), H(0), H(1),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3),
    H(0), H(1), H(0), H(1),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3),
    H(0), H(1), H(0), H(1),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3),
    H(0), H(1), H(0), H(1),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3),
    H(0), H(1),
  )

  val rand8: Circuit = List(
    H(0), H(1), H(2), H(3), H(4), H(5),
    H(6), H(7),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 7), CX(6, 7), CX(1, 3),
    H(5), H(6), H(7),
    CCX(1, 2, 7), CCX(3, 6, 7), X(1), CX(1, 3),
    H(1), H(3), H(4),
    CCX(2, 5, 3), CX(6, 7), CX(2, 4), CX(1, 3),
    H(1), H(2), H(3), H(4), H(5), H(6), H(7),
    X(0), CX(6, 7), CX(4, 1), CX(1, 3),
  )

  val rand16: Circuit = List(
    H(0), H(1), H(2), H(3), H(4), H(5),
    H(6), H(7), H(8), H(9), H(10),
    H(11), H(12), H(13), H(14), H(15),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 10), CX(6, 7), CX(1, 3),
    CX(11, 8), CX(14, 3), CX(1, 12), CX(9, 3), CX(7, 10), CX(15, 4), CX(1, 13),
    H(11), H(12), H(13), H(14),
    CCX(11, 2, 7), CCX(3, 6, 8), X(1), CX(1, 3),
    CCX(2, 5, 10), CX(6, 7), CX(12, 4), CX(1, 3),
    H(1), H(2), H(3), H(4), H(5), H(6), H(7), H(8),
    X(10), CX(6, 7), CX(12, 4), CX(1, 3),
  )

  val rand20: Circuit = List(
    H(0), H(1), H(2), H(3), H(4), H(5), H(6), H(7), H(8), H(9), H(10),
    H(11), H(12), H(13), H(14), H(15), H(16), H(17), H(18), H(19),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 10), CX(6, 7), CX(18, 4), CX(1, 3),
    CX(11, 8), CX(14, 3), CX(1, 12), CX(9, 3), CX(7, 10), CX(16, 5), CX(19, 4), CX(1, 13),
    H(11), H(12), H(13), H(14), H(15), H(16), H(17), H(18), H(19),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 10), CX(6, 7), CX(18, 4), CX(1, 3),
    /*
    H(0), H(1), H(2), H(3), H(4), H(5), H(6), H(7), H(8), H(9), H(10),
    CX(11, 8), CX(14, 3), CX(1, 12), CX(9, 3), CX(7, 10), CX(16, 5), CX(19, 4), CX(1, 13),
    H(0), H(1), H(2), H(3), H(4), H(5), H(6), H(7), H(8), H(9), H(10),
    H(11), H(12), H(13), H(14), H(15), H(16), H(17), H(18), H(19),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 10), CX(6, 7), CX(18, 4), CX(1, 3),
    CX(11, 8), CX(14, 3), CX(1, 12), CX(9, 3), CX(7, 10), CX(16, 5), CX(19, 4), CX(1, 13),
    H(11), H(12), H(13), H(14), H(15), H(16), H(17), H(18), H(19),
    CX(0, 2), CX(0, 3), CX(1, 2), CX(1, 3), CX(5, 10), CX(6, 7), CX(18, 4), CX(1, 3),
    H(0), H(1), H(2), H(3), H(4), H(5), H(6), H(7), H(8), H(9), H(10),
    CX(11, 8), CX(14, 3), CX(1, 12), CX(9, 3), CX(7, 10), CX(16, 5), CX(19, 4), CX(1, 13),
     */
  )

  // 2 qubits
  // XXX: it needs to _only_ measure q0,
  // but I don't know how to do that yet.
  val DeutschJozsa: Circuit = List(
    X(1),
    H(0), H(1),
    H(0),
  )
}
