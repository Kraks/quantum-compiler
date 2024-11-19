package quantum.schrodinger.staged

import math.pow

import lms.core._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import utils.time
import scala.sys.process._

import quantum._
import quantum.circuit.Syntax._
import quantum.schrodinger._
import quantum.schrodinger.Matrix._
import quantum.schrodinger.gate.{Gate, _}

abstract class AbsMat
abstract class AbsVec

abstract class DslDriverPy[A:Manifest,B:Manifest] extends DslSnippet[A,B] with DslExp { q =>
  val codegen = new DslGen { val IR: q.type = q }
  lazy val (code, statics) = {
    val source = new java.io.ByteArrayOutputStream()
    val statics = codegen.emitSource[A,B](wrapper, "Snippet", new java.io.PrintStream(source))
    (source.toString, statics)
  }
  lazy val f = {
    val (c1,s1) = (code,statics);
    val out = new java.io.PrintStream("snippet.py")
    out.println(code)
    out.close
    val pb: ProcessBuilder = s"python3 snippet.py"
    (a: A) => { pb.lines.foreach(Console.println _) }
  }
  def precompile: Unit = f
  def precompileSilently: Unit = lms.core.utils.devnull(f)
  def eval(x: A): Unit = { val f1 = f; time("eval")(f1(x)) }
}

abstract class StagedHLSchrodinger extends DslDriverPy[Int, Unit] with SchrodingerInterpreter { q =>
  override val codegen = new DslGen {
    val IR: q.type = q
    lazy val prelude = """
import numpy as np
from numpy import kron, matmul, array, eye, absolute, real, imag, log2, zeros, sqrt
SWAP = array([
  [1, 0, 0, 0],
  [0, 0, 1, 0],
  [0, 1, 0, 0],
  [0, 0, 0, 1]
])
CNOT = array([
  [1, 0, 0, 0],
  [0, 1, 0, 0],
  [0, 0, 0, 1],
  [0, 0, 1, 0]
])
isq2 = 1.0 / (2.0 ** 0.5)
H = isq2 * array([
  [1,  1],
  [1, -1],
])
def Id(i): return eye(i, dtype=complex)
def Init(i):
  s = zeros(2 ** i, dtype=complex)
  s[0] = 1
  return s
def print_binary(index, size_sqrt):
  bin_format = f"0{int(log2(size_sqrt**2))}b"
  binary = format(index, bin_format)
  print(f"{binary}", end="")
def print_result(arr, size):
  print("[", end="")
  for i in range(size):
    if absolute(real(arr[i])) < 1e-18 and imag(arr[i]) == 0.0:
      continue
    print(arr[i], end="")
    print("|", end="")
    print_binary(i, sqrt(size))
    print("⟩", end="")
    if i < size - 1:
      print(", ", end="")
  print("]")
"""
    lazy val initInput: String  = ""
    lazy val procOutput: String = ""

    override def remap(m: Manifest[_]) = {
      if (m.runtimeClass.getName.endsWith("AbsVec")) "AbsVec"
      else if (m.runtimeClass.getName.endsWith("AbsMat")) "AbsMat"
      else super.remap(m)
    }

    override def shallow(n: Node): Unit = n match {
      case n @ Node(s, "kron", List(x, y), _) => es"kron($x, $y)"
      case n @ Node(s, "matvecprod", List(x, y), _) => es"matmul($x, $y)"
      case n @ Node(s, "init-state", List(Backend.Const(i: Int)), _) => es"Init($i)"
      case n @ Node(s, "H", _, _) => es"H"
      case n @ Node(s, "SWAP", _, _) => es"SWAP"
      case n @ Node(s, "CNOT", _, _) => es"CNOT"
      case _ => super.shallow(n)
    }
    override def traverse(n: Node): Unit = n match {
      case n @ Node(s, "copy", List(from, to), _) =>
        esln"$to = $from;"
      case n @ Node(s,"P",List(x),_) => esln"print_result($x, 2 ** $size)"
      case _ => super.traverse(n)
    }
    override def quoteBlockP(prec: Int)(f: => Unit) = {
      def wraper(numStms: Int, l: Option[Node], y: Block)(f: => Unit) = {
        val paren = numStms == 0 && l.map(n => precedence(n) < prec).getOrElse(false)
        if (paren) emit("(") //else if (numStms > 0) emitln("{")
        f
        if (y.res != Const(())) { shallow(y.res) }
        emit(quoteEff(y.eff))
        if (paren) emit(")") //else if (numStms > 0) emit("\n}")
      }
      withWraper(wraper _)(f)
    }
    override def emitValDef(n: Node): Unit = {
      if (dce.live(n.n)) emit(s"${quote(n.n)} = ");
      shallow(n); emitln()
    }
    override def emitAll(g: Graph, name: String)(m1: Manifest[_], m2: Manifest[_]): Unit = {
      val ng = init(g)
      val arg = quote(g.block.in.head)
      val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
      val (ms1, ms2) = (remap(m1), remap(m2))
      emitln(prelude)
      emitln("#############")
      quoteBlock(apply(ng))
    }
  }

  implicit class AbsMatOps(x: Rep[AbsMat]) {
    def ⊗(y: Rep[AbsMat]): Rep[AbsMat] =
      Wrap[AbsMat](Adapter.g.reflectWrite("kron", Unwrap(x), Unwrap(y))(Adapter.CTRL))
    def *(y: Rep[AbsVec]): Rep[AbsVec] =
      Wrap[AbsVec](Adapter.g.reflectWrite("matvecprod", Unwrap(x), Unwrap(y))(Adapter.CTRL))
  }

  type State = Rep[AbsVec]
  lazy val state: State = Wrap[AbsVec](Adapter.g.reflectWrite("init-state", Unwrap(unit(size)))(Adapter.CTRL))

  def op(g: Gate, i: Int): Unit = {
    val iLeft  = Wrap[AbsMat](Adapter.g.reflect("Id", Unwrap(unit(pow(2, i).toInt))))
    val iRight = Wrap[AbsMat](Adapter.g.reflect("Id", Unwrap(unit(pow(2, size-i-g.arity).toInt))))
    val gate = Wrap[AbsMat](Adapter.g.reflectWrite(g.id)(Adapter.CTRL))
    val buf = iLeft ⊗ gate ⊗ iRight * state
    Adapter.g.reflectWrite("copy", Unwrap(buf), Unwrap(state))(Adapter.CTRL)
  }

  def snippet(n: Rep[Int]): Rep[Unit] = {
    circuit
    println(state)
  }

  def evalCircuit: Unit = eval(0)
}

object TestStagedHLSchrodinger {
  def main(args: Array[String]): Unit = {

    val driver = new StagedHLSchrodinger {
      val size = 4
      def circuit: Unit = {
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

    driver.evalCircuit
  }
}
