package quantum

// Quantum circuit evaluator
// Following the Scheme Pearl paper on Quantum Continuation by Choudhury, Agapiev and Sabry

import scala.util.continuations._
import Syntax._

object QuantumContSim {
  case class State(d: Double, bs: Vector[Boolean]) {
    def toMap: Map[Vector[Boolean], Double] = Map(bs -> d)
  }
  object State {
    def apply(i: Int): State = State(1.0, Vector.fill(i)(false))
  }

  // Accumulate states and their probability amplitudes
  type Ans = Map[Vector[Boolean], Double]

  val hscale: Double = 1.0 / math.sqrt(2.0)

  def isSet(bs: Vector[Boolean], x: Exp): Boolean = x match {
    case Wire(pos) => bs(pos)
    case Bit(b) => b
  }

  def neg(bs: Vector[Boolean], x: Exp): Vector[Boolean] = x match {
    case Wire(pos) => bs.updated(pos, !bs(pos))
  }

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

  def prettyPrint(m: Ans): Unit = {
    m.filter(kv => math.abs(kv._2) > 0.001).foreach { case (k, v) =>
      val p = (if (v > 0) "+" else "") + f"$v%.3f"
      val vs = k.map(x => if (x) "1" else "0").mkString
      print(s"$p|$vs⟩ ")
    }
  }

  def main(args: Array[String]): Unit = {
    import Examples._
    val N = 0
    val (_, t) = Utils.time {
      for (i <- 0 to N) {
        //prettyPrint(runCircuit(simon, State(4)))
        prettyPrint(runCircuit(DeutschJozsa, State(2)))
        println()
      }
    }
    //println(s"$t sec")
  }
}


