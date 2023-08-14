package quantum.schrodinger

import math.pow
import quantum.utils._
import quantum.schrodinger.gate._
import quantum.schrodinger.Matrix._

// Unstaged Schrodinger-style simulation

abstract class UnstagedSchrodinger(val size: Int) extends SchrodingerInterpreter {
  type State = Array[Complex]
  
  var state: State = Matrix.zerosVec(pow(2, size).toInt)
  state(0) = 1 // all deterministically zero

  def evalCircuit: Unit = {
    circuit
    prettyPrintSummary
  }

  def setState(s: State) = {
    assert(state.size == s.size, "incompatible size");
    state = s
  }

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

  def summary: List[(String, Complex)] = {
    state.toList.zipWithIndex
      .map({ case (s, i) =>
        val bin = Integer.toBinaryString(i)
        ("0" * (size - bin.length) + bin, s)
      })
      .filter(_._2 != (0: Complex))
  }

  def prettyPrintSummary: Unit = {
    summary.foreach { case (s, d) =>
      println(s"${d.prettyPrint}|$s⟩")
    }
  }
}

object TestUnstagedSchrodinger {
  def main(args: Array[String]): Unit = {
    val q = new UnstagedSchrodinger(4) {
      def circuit: Unit = {
        H(0)
        H(1)
        SWAP(0) // swap 0 and 1
        CNOT(1) // CNOT(1, 2)
        SWAP(2) // swap 2 and 3
        CNOT(1) // CNOT(1, 2)
        SWAP(0)
        SWAP(1)
        CNOT(2)
        SWAP(1)
        CNOT(1)
        H(0)
        H(1)
      }
    }
    Utils.time { q.evalCircuit }

  }
}
