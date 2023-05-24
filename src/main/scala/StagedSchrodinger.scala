package quantum.tensor.staged

import math.pow
import quantum._
import quantum.Syntax._

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import lms.core.Backend._

object Matrix {
  case class Complex(re: Double, im: Double) {
    override def toString = s"{$re, $im}"
      //re + (if (im < 0) "-" + -im else "+" + im) + "*i"
    def +(c: Complex) = Complex(re + c.re, im + c.im)
    def -(c: Complex) = Complex(re - c.re, im - c.im)
    def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)
  }

  implicit def fromDouble(d: Double): Complex = Complex(d, 0)

  import Complex._

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
      //println(s"${A.dim} ⊗ ${B.dim} = ${result.dim}")
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

class StagedSchrodinger(circuit: Circuit, size: Int) extends DslDriverCPP[Array[Complex], Unit] with ComplexOps { q =>
  override val codegen = new QCodeGen with CppCodeGen_Complex {
    val IR: q.type = q
    override val initInput: String = s"""
    |  Complex* input = (Complex*)malloc(${pow(size, 2)} * sizeof(Complex));
    |  input[0] = {1, 0};
    |""".stripMargin
    override val procOutput: String = s"printArray(input, ${pow(size, 2)});";
    override lazy val prelude = """
    |using namespace std::chrono;
    |typedef struct Complex { double re; double im; } Complex;
    |void printComplex(Complex* c) { printf("%.3f + %.3fi", c->re, c->im); }
    |void printArray(Complex arr[], int size) {
    |  printf("[");
    |  for (int i = 0; i < size; i++) {
    |    printComplex(arr+i);
    |    if (i < size - 1) { printf(", "); }
    |  }
    |  printf("]\n");
    |}
    """.stripMargin
  }
  override val compilerCommand = "g++ -std=c++20 -O3"
  override val sourceFile = "snippet.cpp"
  override val executable = "./snippet"

  def unrollIf(c: Boolean, r: Range) = new {
    def foreach(f: Rep[Int] => Rep[Unit]) = {
      if (c) for (j <- (r.start until r.end): Range) f(j)
      else for (j <- (r.start until r.end): Rep[Range]) f(j)
    }
  }

  def matVecProd(a0: Array[Array[Complex]], v: Rep[Array[Complex]], des: Rep[Array[Complex]]): Unit = {
    val n = a0.length
    val a = staticData(a0)
    for (i <- (0 until n): Range) {
      des(i) = 0.0
      val sparse = a0(i).count(_ != (0: Complex)) < 0.5 * a0(i).size
      System.out.println(s"sparsity: ${a0(i).toList} $sparse")
      for (j <- unrollIf(sparse, 0 until a0(0).size)) {
        des(i) = des(i) + a(i).apply(j) * v(j)
      }
    }
  }

  def sizeof(s: String): Int = s match {
    case "int" => 4
    case "int64" => 8
    case "double" => 8
    case "Complex" => sizeof("double") * 2
  }

  def op(g: Gate, i: Int, state: Rep[Array[Complex]], des: Rep[Array[Complex]]): Unit = {
    val iLeft = Matrix.identity(pow(2, i).toInt)
    val iRight = Matrix.identity(pow(2, size - i - g.arity).toInt)
    matVecProd(iLeft ⊗ g.m ⊗ iRight, state, des)
    // XXX: can we eliminate this copy?
    des.copyToArray(state, 0, pow(size, 2).toInt * sizeof("Complex"))
  }

  def snippet(input: Rep[Array[Complex]]): Rep[Unit] = {
    val buf = NewArray[Complex](pow(size, 2).toInt)
    op(Gate.H, 0, input, buf)
    op(Gate.CNOT, 0, input, buf)
    op(Gate.S, 0, input, buf)
    op(Gate.T, 0, input, buf)
  }

}

object TestStagedSchrodinger {
  def main(args: Array[String]): Unit = {
    val driver = new StagedSchrodinger(List(), 2)
    println(driver.code)
    driver.eval(Array())
  }
}
