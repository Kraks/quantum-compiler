package quantum

// The quantum circuit compiler written in CPS, generate C code

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import lms.core.Backend._

import scala.util.continuations._
import scala.collection.immutable.{List => SList}

import Syntax.{Exp => QExp, _}

@virtualize
trait QCState extends Dsl {
  lazy val hscale: Rep[Double] = 1.0 / math.sqrt(2.0)

  abstract class Bits

  object Bits {
    def apply(bs: List[Boolean]): Rep[Bits] = {
      val sbs = bs.map(Backend.Const(_))
      Wrap[Bits](Adapter.g.reflect("new_bits", sbs: _*))
    }
  }

  implicit class BitsOps(bs: Rep[Bits]) {
    def apply(i: Int): Rep[Boolean] = Unwrap(bs) match {
      case Adapter.g.Def("new_bits", xs) =>
        Wrap[Boolean](Backend.Const(xs(i).asInstanceOf[Backend.Const].x.asInstanceOf[Boolean]))
      case _ => Wrap[Boolean](Adapter.g.reflect("bits_get", Unwrap(bs), Backend.Const(i)))
    }
    def set(i: Int, v: Rep[Boolean]): Rep[Bits] =
      Wrap[Bits](Adapter.g.reflect("bits_set", Unwrap(bs), Backend.Const(i), Unwrap(v)))
  }

  abstract class State

  object State {
    def apply(d: Rep[Double], bs: Rep[Bits]): Rep[State] =
      Wrap[State](Adapter.g.reflect("new_state", Unwrap(d), Unwrap(bs)))
    def apply(size: Int): Rep[State] = apply(1.0, Bits(List.fill(size)(false)))
  }

  implicit class StateOps(s: Rep[State]) {
    def d: Rep[Double] = Unwrap(s) match {
      case Adapter.g.Def("new_state", SList(d: Backend.Exp, _)) => Wrap[Double](d)
      case _                                                    => Wrap[Double](Adapter.g.reflect("state_d", Unwrap(s)))
    }
    def bs: Rep[Bits] = Unwrap(s) match {
      case Adapter.g.Def("new_state", SList(_, bs: Backend.Exp)) => Wrap[Bits](bs)
      case _                                                     => Wrap[Bits](Adapter.g.reflect("state_bs", Unwrap(s)))
    }
  }

  def isSet(bs: Rep[Bits], x: QExp): Rep[Boolean] = x match {
    case Wire(pos) => bs(pos)
    case Bit(b)    => b
  }

  def neg(bs: Rep[Bits], x: QExp): Rep[Bits] = x match {
    case Wire(pos) => bs.set(pos, !bs(pos))
  }
}

@virtualize
trait QCompilerCPS extends QCState {
  type Ans = Unit

  val genCircuitInfo = true

  def info(s: String): Unit = if (genCircuitInfo) unchecked[Unit](s) else ()

  def summarize(s: Rep[State]): Rep[Ans] =
    Wrap[Ans](Adapter.g.reflectWrite("summarize", Unwrap(s))(Adapter.CTRL))

  def evalGate(g: Gate, v: Rep[State], k: Rep[State] => Rep[Ans]): Rep[Ans] = {
    info(s"// $g")
    val repK: Rep[State => Ans] = topFun(k)
    // val repK: Rep[State => Ans] = Wrap[State => Ans](__topFun(k, 1, xn => Unwrap(k(Wrap[State](xn(0)))), "inline"))
    g match {
      case CCX(x, y, z) =>
        if (isSet(v.bs, x) && isSet(v.bs, y)) repK(State(v.d, neg(v.bs, z))) else repK(v)
      case H(x) =>
        if (isSet(v.bs, x)) {
          repK(State(hscale * v.d, neg(v.bs, x)))
          repK(State(-1.0 * hscale * v.d, v.bs))
        } else {
          repK(State(hscale * v.d, neg(v.bs, x)))
          repK(State(hscale * v.d, v.bs))
        }
    }
  }

  def evalCircuit(c: Circuit, v: Rep[State], k: Rep[State] => Rep[Ans]): Rep[Ans] =
    if (c.isEmpty) k(v) else evalGate(c.head, v, s => evalCircuit(c.tail, s, k))

  def runCircuit(c: Circuit, v: Rep[State]): Rep[Ans] = evalCircuit(c, v, summarize)

}

abstract class QCDriver[A: Manifest, B: Manifest] extends DslDriverCPP[A, B] { q =>
  val circuitSize: Int
  val repeat: Int = 1

  override val compilerCommand = "g++ -std=c++20 -O3"
  override val sourceFile      = "snippet.cpp"
  override val executable      = "./snippet"
  override val codegen = new DslGenCPP {
    val IR: q.type = q
    registerHeader("<array>")
    registerHeader("<map>")
    registerHeader("<cmath>")
    registerHeader("<iostream>")
    registerHeader("<chrono>")

    // TODO: consider using std::bitset
    // TODO: instead of using a map as summary, we could allocate
    // an array of 2^n elements to store the prob amplitudes of each
    // possible states.
    lazy val prelude: String = s"""
    |using namespace std::chrono;
    |typedef std::array<bool, $circuitSize> Bits;
    |typedef struct State { double d; Bits bs; } State;
    |Bits bits_set(const Bits& bs, size_t i, bool v) { Bits res = bs; res[i] = v; return res; }
    |void print_state(State s) {
    |  std::cout << s.d << "|";
    |  for (int i = 0; i < $circuitSize; i++) std::cout << (s.bs[i] ? "1" : "0");
    |  std::cout << "⟩\\n";
    |}
    |struct BitsCmp {
    |  bool operator()(const Bits& lhs, const Bits& rhs) const {
    |    for (size_t i = 0; i < $circuitSize; i++) { if (lhs[i] < rhs[i]) return true; }
    |    return false;
    |  }
    |};
    |std::map<Bits, double> summary;
    |void summarize(State s) {
    |  if (summary.contains(s.bs)) { summary[s.bs] = summary[s.bs] + s.d; }
    |  else { summary[s.bs] = s.d; }
    |}
    |void print_summary() {
    |  for (const auto& [bs, d] : summary) {
    |    if (abs(d) < 0.0001) continue;
    |    std::cout << d << "|";
    |    for (int i = 0; i < $circuitSize; i++) std::cout << (bs[i] ? "1" : "0");
    |    std::cout << "⟩\\n";
    |  }
    |  std::cout << \"#results: \" << summary.size() << \"\\n\";
    |}
    """.stripMargin

    override def remap(m: Manifest[_]): String = {
      if (m.toString.endsWith("$State")) "State"
      else if (m.toString.endsWith("$Bits")) "Bits"
      else super.remap(m)
    }

    override def shallow(n: Node): Unit = n match {
      case Node(s, "new_bits", xs, _)              => es"""{${xs.mkString(", ")}}"""
      case Node(s, "bits_get", bs :: i :: Nil, _)  => es"$bs[$i]"
      case Node(s, "new_state", d :: bs :: Nil, _) => es"{ $d, $bs }"
      case Node(s, "state_d", st :: Nil, _)        => es"$st.d"
      case Node(s, "state_bs", st :: Nil, _)       => es"$st.bs"
      case _                                       => super.shallow(n)
    }

    override def emitAll(g: Graph, name: String)(m1: Manifest[_], m2: Manifest[_]): Unit = {
      val ng  = init(g)
      val efs = ""
      val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
      val src = run(name, ng)
      emitDefines(stream)
      emitHeaders(stream)
      emit(prelude)
      emitFunctionDecls(stream)
      emitDatastructures(stream)
      emitFunctions(stream)
      emitInit(stream)
      emitln(s"\n/**************** $name ****************/")
      emit(src)
      emitln(s"""
      |int main(int argc, char *argv[]) {
      |  auto start = high_resolution_clock::now();
      |  for (size_t i = 0; i < $repeat; i++) { summary.clear(); $name(0); }
      |  auto end = high_resolution_clock::now();
      |  auto duration = duration_cast<microseconds>(end - start);
      |  std::cout << std::fixed;
      |  std::cout << "time: ";
      |  std::cout << (duration_cast<microseconds>(duration).count() / 1.0e6) << "s\\n";
      |  print_summary();
      |  return 0;
      |}""".stripMargin)
    }
  }
}

object TestQC {
  import Examples._

  def main(args: Array[String]): Unit = {
    val snippet = new QCDriver[Int, Unit] with QCompilerCPS {
      val circuitSize: Int                = 4
      override val repeat: Int            = 1
      def snippet(s: Rep[Int]): Rep[Unit] = runCircuit(simon, State(circuitSize))
    }
    snippet.eval(0)
  }
}
