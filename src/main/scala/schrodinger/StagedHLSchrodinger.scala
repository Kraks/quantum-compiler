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

abstract class AbsMat
abstract class AbsVec

abstract class StagedHLSchrodinger extends DslDriverCPP[Int, AbsVec] with SchrodingerInterpreter { q =>
  override val codegen = new QCodeGen with CppCodeGen_Complex {
    val IR: q.type = q
    override lazy val prelude = ""
    override lazy val initInput: String  = ""
    override lazy val procOutput: String = ""
    override def remap(m: Manifest[_]) = {
      if (m.runtimeClass.getName.endsWith("AbsVec")) "AbsVec"
      else if (m.runtimeClass.getName.endsWith("AbsMat")) "AbsMat"
      else super.remap(m)
    }
    override def shallow(n: Node): Unit = n match {
      case n @ Node(s, "kron", List(x, y), _) => es"$x ⊗ $y"
      case n @ Node(s, "matvecprod", List(x, y), _) => es"$x * $y"
      case _ => super.shallow(n)
    }
    override def traverse(n: Node): Unit = n match {
      case n @ Node(s, "copy", List(from, to), _) =>
        esln"$to = $from;"
      case _ => super.traverse(n)
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

  def snippet(n: Rep[Int]): State = {
    circuit
    state
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

    println(driver.code)
  }
}
