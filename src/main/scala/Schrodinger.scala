package quantum.tensor

import math.pow

case class Complex(re: Double, im: Double) {
  override def toString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
  def +(c: Complex) = Complex(re + c.re, im + c.im)
  def -(c: Complex) = Complex(re - c.re, im - c.im)
  def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)
}

object Complex {
  implicit def fromDouble(d: Double) = Complex(d, 0)
}

import Complex._

object Matrix {
  type Matrix = Array[Array[Complex]]

  def identity(n: Int): Matrix = {
    val result = zeros(n)
    for (i <- 0 until n) {
      result(i)(i) = 1
    }
    result
  }
  def zeros(n: Int): Matrix = {
    val result = Array.ofDim[Complex](n, n)
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        result(i)(j) = 0
      }
    }
    result
  }
  def zerosVec(n: Int): Array[Complex] = Array.fill(n)(0)

  def prettyPrint(A: Matrix): String = {
    val sb = new StringBuilder
    val nRows = A.size
    val nCols = A(0).size
    sb ++= "["
    for (i <- 0 until nRows) {
      sb ++= (new ArrayOps(A(i))).toString
      sb ++= "\n"
    }
    sb ++= "]\n"
    sb.toString
  }

  implicit class ArrayOps(A: Array[Complex]) {
    override def toString: String = {
      val sb = new StringBuilder
      sb ++= "["
      for (i <- 0 until A.size) sb ++= s"${A(i)}, "
      sb ++= "]"
      sb.toString
    }
  }

  implicit class MatrixOps(A: Matrix) {
    def pPrint: String = prettyPrint(A)
    // Kronecker product
    def ⊗(B: Matrix): Matrix = {
      val nRowsA = A.size
      val nColsA = A(0).size
      val nRowsB = B.size
      val nColsB = B(0).size
      val result = Array.ofDim[Complex](nRowsA * nRowsB, nColsA * nColsB)
      for (i <- 0 until nRowsA; j <- 0 until nColsA) {
        for (k <- 0 until nRowsB; l <- 0 until nColsB) {
          result(i * nRowsB + k)(j * nColsB + l) = A(i)(j) * B(k)(l)
        }
      }
      //println(s"${A.dim} ⊗ ${B.dim} = ${result.dim}")
      result
    }
    // Matrix multiplication
    def *(B: Matrix): Matrix = {
      val nRowsA = A.size
      val nColsA = A(0).size
      val nRowsB = B.size
      val nColsB = B(0).size
      require(nColsA == nRowsB, s"dimension error nColsA=$nColsA, nRowsB=$nRowsB \n${prettyPrint(A)} * ${prettyPrint(B)}")
      val result = Array.ofDim[Complex](nRowsA, nColsB)
      for (i <- 0 until nRowsA) {
        for (j <- 0 until nColsB) {
          var sum: Complex = 0.0
          for (k <- 0 until nColsA) {
            sum += A(i)(k) * B(k)(j)
          }
          result(i)(j) = sum
        }
      }
      //println(s"${A.dim} * ${B.dim} = ${result.dim}")
      result
    }
    // Matrix-vector product
    def *(V: Array[Complex]): Array[Complex] = {
      val nRowsA = A.size
      val nColsA = A(0).size
      require(nColsA == V.size, s"dimension error")
      val result = zerosVec(nRowsA)
      for (i <- 0 until nRowsA) {
        for (j <- 0 until nColsA) {
          result(i) += A(i)(j) * V(j)
        }
      }
      //println(s"${A.dim} * ${V.size} = ${result.size}")
      result
    }
    def dim: (Int, Int) = (A.size, A(0).size)
  }
}

import Matrix._

case class Gate(id: String, m: Matrix) {
  def arity: Int = pow(m.size, 0.5).toInt
}
object Gate {
  val isq2 = 1.0 / pow(2.0, 0.5) 
  val H = Gate("H",
    Array(
      Array(isq2, isq2),
      Array(isq2, -isq2)
    ))
  val NOT = Gate("NOT",
    Array(
      Array(0, 1),
      Array(1, 0)
    ))
  val CNOT = Gate("CNOT", 
    Array(
      Array(1, 0, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 0, 0, 1),
      Array(0, 0, 1, 0),
    ))
  val S = Gate("S",
    Array(
      Array(1, 0),
      Array(0, Complex(0, 1))
    ))
  val T = Gate("T",
    Array(
      Array(1, 0),
      Array(0, isq2 + isq2 * Complex(0, 1))
    ))
}

class QState(var state: Array[Complex], size: Int) {
  def op(g: Gate, i: Int) = {
    //println(pow(2, i).toInt)
    val iLeft = Matrix.identity(pow(2, i).toInt)
    //println(iLeft.pPrint)
    //println(pow(2, size - i - g.arity).toInt)
    val iRight = Matrix.identity(pow(2, size - i - g.arity).toInt)
    //println(iRight.pPrint)
    state = iLeft ⊗ g.m ⊗ iRight * state
  }
  def H(i: Int): Unit = op(Gate.H, i)
  def NOT(i: Int): Unit = op(Gate.NOT, i)
  def CNOT(i: Int): Unit = op(Gate.CNOT, i)
  def S(i: Int): Unit = op(Gate.S, i)
  def T(i: Int): Unit = op(Gate.T, i)
}

object QState {
  def apply(n: Int): QState = {
    val s = Matrix.zerosVec(pow(n, 2).toInt)
    s(0) = 1 // all deterministically zero
    new QState(s, n)
  }
}
