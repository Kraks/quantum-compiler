package quantum.tensor

import math.pow
import quantum.tensor.gate._
import quantum.tensor.Matrix._

class QState(var state: Array[Complex], size: Int) {
  def op(g: Gate, i: Int) = {
    // println(pow(2, i).toInt)
    val iLeft = Matrix.identity(pow(2, i).toInt)
    // println(iLeft.pPrint)
    // println(pow(2, size - i - g.arity).toInt)
    val iRight = Matrix.identity(pow(2, size - i - g.arity).toInt)
    // println(iRight.pPrint)
    state = iLeft ⊗ g.m ⊗ iRight * state
    // tiling, auto vec
  }
  def H(i: Int): Unit    = op(Gate.H, i)
  def NOT(i: Int): Unit  = op(Gate.NOT, i)
  def CNOT(i: Int): Unit = op(Gate.CNOT, i)
  def S(i: Int): Unit    = op(Gate.S, i)
  def T(i: Int): Unit    = op(Gate.T, i)
}

object QState {
  def apply(n: Int): QState = {
    val s = Matrix.zerosVec(pow(n, 2).toInt)
    s(0) = 1 // all deterministically zero
    new QState(s, n)
  }
}
