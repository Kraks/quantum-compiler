package quantum.schrodinger.staged

import math.pow

import lms.core._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import quantum._
import quantum.circuit.Syntax._
import quantum.schrodinger.Matrix
import quantum.schrodinger.Matrix._
import quantum.schrodinger.Complex
import quantum.schrodinger.gate.{Gate, _}

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
  override val sourceFile      = "snippet.cpp"
  override val executable      = "./snippet"

  def unrollIf(c: Boolean, r: Range) = new {
    def foreach(f: Rep[Int] => Rep[Unit]) = {
      if (c) for (j <- (r.start until r.end): Range) f(j)
      else for (j   <- (r.start until r.end): Rep[Range]) f(j)
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
    case "int"     => 4
    case "int64"   => 8
    case "double"  => 8
    case "Complex" => sizeof("double") * 2
  }

  def op(g: Gate, i: Int, state: Rep[Array[Complex]], des: Rep[Array[Complex]]): Unit = {
    val iLeft  = Matrix.identity(pow(2, i).toInt)
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
