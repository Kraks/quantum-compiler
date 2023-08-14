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
import quantum.schrodinger._
import quantum.schrodinger.Matrix._
import quantum.schrodinger.gate.{Gate, _}

abstract class StagedSchrodinger extends DslDriverCPP[Array[Complex], Array[Complex]] with ComplexOps with SchrodingerInterpreter { q =>
  override val codegen = new QCodeGen with CppCodeGen_Complex {
    registerHeader("<cmath>")

    val IR: q.type = q
    override lazy val initInput: String = s"""
    |  Complex* input = (Complex*) malloc(${pow(2, size)} * sizeof(Complex));
    |  input[0] = {1, 0};
    |""".stripMargin
    override lazy val procOutput: String = s"printResult(input, ${pow(2, size)});";
    override lazy val prelude = """
    |using namespace std::chrono;
    |typedef struct Complex { double re; double im; } Complex;
    |void printComplex(Complex* c) {
    |  if (c->im == 0.0) { printf("%.3f", c->re); }
    |  else { printf("%.3f + %.3fi", c->re, c->im); }
    |}
    |void printBinary(uint64_t n, size_t size) {
    |  for (int i = size - 1; i >= 0; i--) {
    |    int shifted = n >> i;
    |    int bit = shifted & 1;
    |    printf("%d", bit);
    |  }
    |}
    |void printResult(Complex arr[], size_t size) {
    |  printf("[");
    |  for (int i = 0; i < size; i++) {
    |    if ((arr+i)->re == 0.0 && (arr+i)->im == 0.0) continue;
    |    printComplex(arr+i);
    |    printf("|");
    |    printBinary(i, sqrt(size));
    |    printf("⟩");
    |    if (i < size - 1) { printf(", "); }
    |  }
    |  printf("]\n");
    |}
    """.stripMargin
    override def traverse(n: Node): Unit = n match {
      case n @ Node(s, "copy", List(from, to), _) =>
        esln"$to = $from;"
      case _ => super.traverse(n)
    }
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
      val sparse = false //a0(i).count(_ != (0: Complex)) < 0.5 * a0(i).size
      // System.out.println(s"sparsity: ${a0(i).toList} $sparse")
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

  type State = Rep[Array[Complex]]
  lazy val buf = NewArray[Complex](pow(2, size).toInt)
  var state: State = _

  @virtualize
  def op(g: Gate, i: Int): Unit = {
    val iLeft  = Matrix.identity(pow(2, i).toInt)
    val iRight = Matrix.identity(pow(2, size - i - g.arity).toInt)
    matVecProd(iLeft ⊗ g.m ⊗ iRight, state, buf)
    // XXX: can we eliminate this copy? Could alternate buf and state
    buf.copyToArray(state, 0, pow(2, size).toInt * sizeof("Complex"))
  }

  def snippet(input: State): State = {
    state = input
    circuit
    state
  }

  def evalCircuit: Unit = eval(scala.Array())
}

object TestStagedSchrodinger {
  def main(args: Array[String]): Unit = {
    val driver = new StagedSchrodinger {
      val size = 4
      def circuit: Unit = {
        // H(0)
        // CNOT(0)
        // S(0)
        // T(0)
        // Simon's problem
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
    println(driver.code)
    driver.evalCircuit
  }
}
