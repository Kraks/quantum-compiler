package quantum.feynman

// Unstaged quantum circuit evaluator in Feynman-style using continuations
// Instead of using shift/reset, this one is directly written in CPS

import quantum.circuit.Syntax._
import quantum.utils.Utils
import EvalState._

import scala.collection.mutable.HashMap

object QuantumEvalCPS {
  type Ans = Unit

  val summary: HashMap[Vector[Boolean], Double] = HashMap()

  def evalGate(g: Gate, s: State, k: State => Ans): Ans =
    g match {
      case CCX(x, y, z) if isSet(s.bs, x) && isSet(s.bs, y) => k(State(s.d, neg(s.bs, z)))
      case CCX(x, y, z)                                     => k(s)
      case H(x) if isSet(s.bs, x) =>
        k(State(hscale * s.d, neg(s.bs, x)))
        k(State(-1.0 * hscale * s.d, s.bs))
      case H(x) =>
        k(State(hscale * s.d, neg(s.bs, x)))
        k(State(hscale * s.d, s.bs))
    }

  def evalCircuit(c: Circuit, s: State, k: State => Ans): Ans =
    if (c.isEmpty) k(s) else evalGate(c.head, s, s => evalCircuit(c.tail, s, k))

  def runCircuit(c: Circuit, s: State): Ans = {
    summary.clear
    evalCircuit(c, s, summarize)
  }

  def summarize(s: State): Unit =
    if (summary.contains(s.bs)) summary(s.bs) = summary(s.bs) + s.d
    else summary(s.bs) = s.d
}
