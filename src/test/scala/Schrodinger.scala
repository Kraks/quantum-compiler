package quantum.schrodinger

import math.pow
import org.scalatest.funsuite.AnyFunSuite

import quantum.schrodinger.Matrix._
import quantum.schrodinger.gate.{Gate, _}

class SchrodingerTest extends AnyFunSuite {
  def checkEq(s1: Array[Complex], s2: Array[Complex]): Boolean = {
    s1.zip(s2).forall { case (c1, c2) => c1 ≈ c2 }
  }
  def checkEq(s1: List[(String, Complex)], s2: List[(String, Complex)]): Boolean = {
    s1.zip(s2).forall { case (c1, c2) => c1._1 == c2._1 && c1._2 ≈ c2._2 }
  }

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
    assert(checkEq(s.state, Array[Complex](Gate.isq2, 0, 0, Complex(-0.5, 0.5))))
  }

  test("Simon") {
    // Note: because CNOT only works on two adjacent wires and stores the result
    // into the second wire, we have to swap the wires before applying CNOT to
    // two non-adjacent wires.
    val s = QState(4)
    s.H(0)
    s.H(1)
    s.SWAP(0) // swap 0 and 1
    s.CNOT(1) // CNOT(1, 2)
    s.SWAP(2) // swap 2 and 3
    s.CNOT(1) // CNOT(1, 2)
    s.SWAP(0)
    s.SWAP(1)
    s.CNOT(2)
    s.SWAP(1)
    s.CNOT(1)
    s.H(0)
    s.H(1)
    assert(
      checkEq(
        s.summary,
        List[(String, Complex)](
          ("0000", 0.5),
          ("0011", 0.5),
          ("1100", 0.5),
          ("1111", -0.5)
        )
      )
    )
  }

  test("Toffoli") {
    var s = QState(3)
    s.CCNOT(0)
    assert(checkEq(s.summary, List[(String, Complex)](("000", 1.0))))

    s = QState(3)
    val b110 = Matrix.zerosVec(pow(2, 3).toInt)
    b110(6) = 1
    s.setState(b110)
    s.CCNOT(0)
    assert(checkEq(s.summary, List[(String, Complex)](("111", 1.0))))

    s = QState(3)
    val b111 = Matrix.zerosVec(pow(2, 3).toInt)
    b111(7) = 1
    s.setState(b111)
    s.CCNOT(0)
    assert(checkEq(s.summary, List[(String, Complex)](("110", 1.0))))
  }
}
