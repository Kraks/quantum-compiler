package quantum

// The quantum circuit compiler written in CPS, generate C code

import lms.core._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import scala.util.continuations._
import scala.collection.immutable.{List => SList}

import quantum.circuit.Syntax.{Exp => QExp, _}
import quantum.circuit.Examples._
import quantum.feynman._
import quantum.feynman.staged._
import quantum.feynman.EvalState.{prettyPrint, State}
import quantum.utils.Utils

object Benchmark {
  val benchmarks: List[(Circuit, Int)] = List(
    // (simon, 4),
    // (rand4, 4),
    // (rand8, 8),
    (rand16, 16)
  )

  def test(ci: (Circuit, Int)): Unit = {
    val (circuit, size) = ci
    println(s"circuit size: ${circuit.size}")
    // warm up
    for (i <- 0 to 5) {
      QuantumEvalCPS.runCircuit(circuit, State(size))
    }
    val (_, t) = Utils.time {
      QuantumEvalCPS.runCircuit(circuit, State(size))
    }
    println(s"$t sec")

    val snippet = new QCDriver[Int, Unit] with QCompilerCPS {
      val circuitSize: Int                = size
      override val repeat: Int            = 1
      def snippet(s: Rep[Int]): Rep[Unit] = runCircuit(circuit, State(circuitSize))
    }
    snippet.eval(0)
  }

  def main(args: Array[String]): Unit = {
    benchmarks.foreach(test)
  }

}
