package quantum

// Quantum circuit evaluator
// Following the Scheme Pearl paper on Quantum Continuation by Choudhury, Agapiev and Sabry

import scala.util.continuations._
import Syntax._
import EvalState._

object QuantumContSim {
  // Accumulate states and their probability amplitudes
  type Ans = Map[Vector[Boolean], Double]

  def collect(x: State, y: State): State @cps[Ans] = shift { k =>
    val a = k(x)
    val b = k(y)
    a.foldLeft(b) { case (m, (k, v)) => m + (k -> (m.getOrElse(k, 0.0)+v)) }
  }

  def evalGate(g: Gate, v: State): State @cps[Ans] = {
    val State(d, bs) = v
    g match {
      case CCX(x, y, z) if isSet(bs, x) && isSet(bs, y) => State(d, neg(bs, z))
      case CCX(x, y, z) => v
      case H(x) if isSet(bs, x) => collect(State(hscale * d, neg(bs, x)), State(-1.0 * hscale * d, bs))
      case H(x) => collect(State(hscale * d, neg(bs, x)), State(hscale * d, bs))
    }
  }

  def evalCircuit(c: Circuit, v: State): State @cps[Ans] =
    if (c.isEmpty) v else evalCircuit(c.tail, evalGate(c.head, v))

  def runCircuit(c: Circuit, v: State): Ans = reset { evalCircuit(c, v).toMap }

}

object TestQContSim {
  import QuantumContSim._

  def main(args: Array[String]): Unit = {
    import Examples._
    val N = 10000
    val (_, t1) = Utils.time {
      for (i <- 0 to N) {
        QuantumContSim.runCircuit(simon, State(4))
        //prettyPrint(QuantumContSim.runCircuit(simon, State(4)))
        //println()
      }
    }
    val (_, t2) = Utils.time {
      for (i <- 0 to N) {
        QuantumEvalCPS.runCircuit(simon, State(4))
        //prettyPrint(QuantumEvalCPS.summary.toMap)
        //println()
      }
    }
    println(s"$t1 sec; $t2 sec")
  }

}

