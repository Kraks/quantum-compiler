package quantum

// Auxiliary definitions for the evaluators

import Syntax._

object EvalState {
  case class State(d: Double, bs: Vector[Boolean]) {
    def toMap: Map[Vector[Boolean], Double] = Map(bs -> d)
  }
  object State {
    def apply(i: Int): State = State(1.0, Vector.fill(i)(false))
  }

  val hscale: Double = 1.0 / math.sqrt(2.0)

  def isSet(bs: Vector[Boolean], x: Exp): Boolean = x match {
    case Wire(pos) => bs(pos)
    case Bit(b) => b
  }

  def neg(bs: Vector[Boolean], x: Exp): Vector[Boolean] = x match {
    case Wire(pos) => bs.updated(pos, !bs(pos))
  }

  def prettyPrint(m: Map[Vector[Boolean], Double]): Unit = {
    m.filter(kv => math.abs(kv._2) > 0.001).foreach { case (k, v) =>
      val p = (if (v > 0) "+" else "") + f"$v%.3f"
      val vs = k.map(x => if (x) "1" else "0").mkString
      print(s"$p|$vs⟩ ")
    }
  }
}
