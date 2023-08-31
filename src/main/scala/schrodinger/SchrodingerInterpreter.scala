package quantum.schrodinger

import quantum.schrodinger.gate.{Gate, _}

trait SchrodingerInterpreter {
  type State
  val size: Int
  def op(g: Gate, i: Int): Unit
  def circuit: Unit
  def evalCircuit: Unit

  def H(i: Int): Unit     = op(Gate.H, i)
  def SWAP(i: Int): Unit  = op(Gate.SWAP, i)
  def NOT(i: Int): Unit   = op(Gate.NOT, i)
  def CNOT(i: Int): Unit  = op(Gate.CNOT, i)
  def CCNOT(i: Int): Unit = op(Gate.CCNOT, i)
  def S(i: Int): Unit     = op(Gate.S, i)
  def T(i: Int): Unit     = op(Gate.T, i)
  def Z(i: Int): Unit     = op(Gate.Z, i)
  def CZ(i: Int): Unit    = op(Gate.CZ, i)
}

