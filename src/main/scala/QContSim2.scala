package quantum

// Quantum circuit evaluator
// Instead of using shift/reset, this one is directly written in CPS

import Syntax._
import EvalState._
import scala.collection.mutable.HashMap

object QuantumEvalCPS {
  type Ans = Unit

  val summary: HashMap[Vector[Boolean], Double] = HashMap()

  def evalGate(g: Gate, v: State, k: State => Ans): Ans = {
    val State(d, bs) = v
    g match {
      case CCX(x, y, z) if isSet(bs, x) && isSet(bs, y) => k(State(d, neg(bs, z)))
      case CCX(x, y, z) => k(v)
      case H(x) if isSet(bs, x) =>
        k(State(hscale * d, neg(bs, x)))
        k(State(-1.0 * hscale * d, bs))
      case H(x) =>
        k(State(hscale * d, neg(bs, x)))
        k(State(hscale * d, bs))
    }
  }

  def evalCircuit(c: Circuit, v: State, k: State => Ans): Ans =
    if (c.isEmpty) k(v) else evalGate(c.head, v, s => evalCircuit(c.tail, s, k))

  def runCircuit(c: Circuit, v: State): Ans = {
    summary.clear
    evalCircuit(c, v, summarize)
  }

  def summarize(v: State): Unit =
    if (summary.contains(v.bs)) summary(v.bs) = summary(v.bs) + v.d
    else summary(v.bs) = v.d
}
