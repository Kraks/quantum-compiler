package quantum.tensor

import math.pow
import org.scalatest.funsuite.AnyFunSuite

import Matrix._

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

class MatrixTest extends AnyFunSuite {

  test("Identity") {
    val m1: Matrix = Array(
      Array(1, 0, 0),
      Array(0, 1, 0),
      Array(0, 0, 1)
    )
    assert(Matrix.identity(3).flatten.toList == m1.flatten.toList)
  }

  test("MatMul") {
    val m1: Matrix = Array(
      Array(1, 0, 1),
      Array(2, 1, 1),
      Array(0, 1, 1),
      Array(1, 1, 2)
    )
    val m2: Matrix = Array(
      Array(1, 2, 1),
      Array(2, 3, 1),
      Array(4, 2, 2)
    )
    val m3: Matrix = Array(
      Array(5, 4, 3),
      Array(8, 9, 5),
      Array(6, 5, 3),
      Array(11, 9, 6)
    )
    assert((m1 * m2).flatten.toList == m3.flatten.toList)
  }

  test("Kronecker") {
    val m1: Matrix = Array(
      Array(1, 2),
      Array(3, 4)
    )
    val m2: Matrix = Array(
      Array(0, 5),
      Array(6, 7)
    )
    val m3: Matrix = Array(
      Array(0, 5, 0, 10),
      Array(6, 7, 12, 14),
      Array(0, 15, 0, 20),
      Array(18, 21, 24, 28)
    )

    assert((m1 ⊗ m2).flatten.toList == m3.flatten.toList)

    val m4: Matrix = Array(
      Array(1, -4, 7),
      Array(-2, 3, 3)
    )
    val m5: Matrix = Array(
      Array(8, -9, -6, 5),
      Array(1, -3, -4, 7),
      Array(2, 8, -8, -3),
      Array(1, 2, -5, -1)
    )
    val m6: Matrix = Array(
      Array(8, -9, -6, 5, -32, 36, 24, -20, 56, -63, -42, 35),
      Array(1, -3, -4, 7, -4, 12, 16, -28, 7, -21, -28, 49),
      Array(2, 8, -8, -3, -8, -32, 32, 12, 14, 56, -56, -21),
      Array(1, 2, -5, -1, -4, -8, 20, 4, 7, 14, -35, -7),
      Array(-16, 18, 12, -10, 24, -27, -18, 15, 24, -27, -18, 15),
      Array(-2, 6, 8, -14, 3, -9, -12, 21, 3, -9, -12, 21),
      Array(-4, -16, 16, 6, 6, 24, -24, -9, 6, 24, -24, -9),
      Array(-2, -4, 10, 2, 3, 6, -15, -3, 3, 6, -15, -3)
    )
    assert((m4 ⊗ m5).flatten.toList == m6.flatten.toList)
  }
}
