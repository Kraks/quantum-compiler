package quantum.schrodinger

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
    val sb    = new StringBuilder
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

  implicit class DoubleOps(scalar: Double) {
    def *(A: Matrix): Matrix = {
      val nRowsA = A.size
      val nColsA = A(0).size
      val result = Array.ofDim[Complex](nRowsA, nColsA)
      for (i <- 0 until nRowsA) {
        for (j <- 0 until nColsA) {
          result(i)(j) = scalar * A(i)(j)
        }
      }
      result
    }
  }

  implicit class MatrixOps(A: Matrix) {
    def pPrint: String = prettyPrint(A)
    // Unstaged Kronecker product
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
      // println(s"${A.dim} ⊗ ${B.dim} = ${result.dim}")
      result
    }
    // Unstaged matrix multiplication
    def *(B: Matrix): Matrix = {
      val nRowsA = A.size
      val nColsA = A(0).size
      val nRowsB = B.size
      val nColsB = B(0).size
      require(
        nColsA == nRowsB,
        s"dimension error nColsA=$nColsA, nRowsB=$nRowsB \n${prettyPrint(A)} * ${prettyPrint(B)}"
      )
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
      // println(s"${A.dim} * ${B.dim} = ${result.dim}")
      result
    }
    // Unstaged matrix-vector product
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
      // println(s"${A.dim} * ${V.size} = ${result.size}")
      result
    }
    def dim: (Int, Int) = (A.size, A(0).size)
  }
}
