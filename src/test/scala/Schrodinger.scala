package quantum.schrodinger

import math.pow
import org.scalatest.funsuite.AnyFunSuite

import quantum.schrodinger.Matrix._
import quantum.schrodinger.gate.{Gate, _}

class SchrodingerTest extends AnyFunSuite {
  test("EPR") {
    val s = QState(2)
    s.H(0)
    s.CNOT(0)
    assert(s.state.toList == List[Complex](Gate.isq2, 0, 0, Gate.isq2))
  }

  test("S&T") {
    val s = QState(2)
    s.H(0)
    s.CNOT(0)
    s.S(0)
    s.T(0)
    // FIXME double comparison
    // assert(s.state.toList == List[Complex](Gate.isq2, 0, 0, Complex(0.5, 0.5)))
  }
}
