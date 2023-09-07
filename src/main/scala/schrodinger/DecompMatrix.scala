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
import quantum.schrodinger.gate.{Gate, _}

import scala.collection.immutable.{List => SList}

trait DecompMatrix { q: Dsl =>
  // basic matrix element, represented as opaque IR node
  abstract class MElem

  def zeros(cols: Int, rows: Int): AtomMat =
    AtomMat(Wrap[MElem](Adapter.g.reflect("zeros", Unwrap(unit(cols)), Unwrap(unit(rows)))))
  def rand(cols: Int, rows: Int): AtomMat =
    AtomMat(Wrap[MElem](Adapter.g.reflect("rand", Unwrap(unit(cols)), Unwrap(unit(rows)))))
  def sqZeros(n: Int): AtomMat = zeros(n, n)
  def id(n: Int): AtomMat =
    AtomMat(Wrap[MElem](Adapter.g.reflect("id", Unwrap(unit(n)))))
  def invId(n: Int): AtomMat =
    AtomMat(Wrap[MElem](Adapter.g.reflect("inv-id", Unwrap(unit(n)))))


  // abstract vector, representing the initial input vector and final result
  abstract class AbsVec

  type State = Rep[AbsVec]
  def mtState(size: Int): State =
    Wrap[AbsVec](Adapter.g.reflectWrite("init-state", Unwrap(unit(size)))(Adapter.CTRL))

  implicit class AbsVecOps(v: Rep[AbsVec]) {
    def size: Int = v match {
      case Adapter.g.Def(_, ops) =>
        val Backend.Const(n: Int) = ops.last
        n
    }
  }

  // abstract matrix, being either an atomic matrix holding an MElem,
  // or a large matrix that can be decomposed to smaller AbsMats
  abstract class AbsMat {
    type T = Int // TODO: should generalize
    def draw(offset: (Int, Int) = (0, 0)): Unit
    def apply(i: Int, j: Int): Rep[T]
    def dim: (Int, Int) /* (cols, rows) */ 
    def ⊗(y: AbsMat): AbsMat
    def *(y: Rep[AbsVec]): Rep[AbsVec]
  }
  case class AtomMat(e: Rep[MElem]) extends AbsMat {
    def draw(offset: (Int, Int)) =
      Adapter.g.reflectWrite("draw", Unwrap(e), Unwrap(unit(offset._1)), Unwrap(unit(offset._2)))(Adapter.CTRL)
    def apply(i: Int, j: Int): Rep[T] = Unwrap(e) match {
      case Adapter.g.Def("zeros", _) => unit(0)
      case Adapter.g.Def("id", _) => if (i == j) unit(1) else unit(0)
      case Adapter.g.Def("inv-id", SList(Backend.Const(n: Int))) => if (j + j == n) unit(1) else unit(0)
      case _ => Wrap[T](Adapter.g.reflect("at", Unwrap(e), Unwrap(unit(i)), Unwrap(unit(j))))
    }
    def dim: (Int, Int) = Unwrap(e) match {
      // The last two operands are always dimensions
      case Adapter.g.Def("zeros", SList(Backend.Const(cols: Int), Backend.Const(rows: Int))) => (cols, rows)
      case Adapter.g.Def("rand", SList(Backend.Const(cols: Int), Backend.Const(rows: Int))) => (cols, rows)
      case Adapter.g.Def("id", SList(Backend.Const(n: Int))) => (n, n)
      case Adapter.g.Def("inv-id", SList(Backend.Const(n: Int))) => (n, n)
      case Adapter.g.Def("kron", SList(_, _, Backend.Const(cols: Int), Backend.Const(rows: Int))) =>
        (cols, rows)
      case _ => System.out.println(e); ???
    }

    def ⊗(y: AbsMat): AbsMat = Unwrap(e) match {
      case Adapter.g.Def("zeros", SList(Backend.Const(p: Int), Backend.Const(q: Int))) =>
        val (m, n) = y.dim
        zeros(p*m, q*n)
      case Adapter.g.Def("id", SList(Backend.Const(h: Int))) =>
        val (m, n) = y.dim
        val zs = List.fill(h)(List.fill(h)(zeros(m, n)))
        DecomposedMat(zs.zipWithIndex.map { case (z, i) => z.updated(i, y) })
      case _ =>
        val (m, n) = this.dim
        val (p, q) = y.dim
        y match {
          case AtomMat(y) =>
            // the kronecker product of two atom matrix is still an atom matrix (represented as opaque IR node)
            val mat = Wrap[MElem](Adapter.g.reflect("kron", Unwrap(e), Unwrap(y), Unwrap(unit(m*p)), Unwrap(unit(q*n))))
            AtomMat(mat)
          case DecomposedMat(m) => DecomposedMat(m.map { row => row.map(this ⊗ _) })
        }
    }
    def *(y: Rep[AbsVec]): Rep[AbsVec] = {
      val (m, n) = this.dim
      val p: Int = y.size
      require(n == p, "dimension error")
      // result vector size m
      Unwrap(e) match {
        case Adapter.g.Def("zeros", SList(Backend.Const(p: Int), Backend.Const(q: Int))) => ???
        case Adapter.g.Def("id", SList(Backend.Const(h: Int))) => ???
        case _ => ???
      }
    }
  }
  case class DecomposedMat(m: List[List[AbsMat]]) extends AbsMat {
    def draw(offset: (Int, Int)) =
      m.foldLeft(offset._2) { case (rowAcc, row) =>
        row.foldLeft(offset._1) { case (colAcc, e) =>
          e.draw((rowAcc, colAcc))
          colAcc + e.dim._1
        }
        rowAcc + row(0).dim._2
      }
    def apply(i: Int, j: Int): Rep[T] = ???
    def dim: (Int, Int) = {
      val cols = m(0).foldLeft(0) { (acc, x) => acc + x.dim._1 }
      val rows = m.map(_.head).foldLeft(0) { (acc, x) => acc + x.dim._2 }
      (cols, rows)
    }
    def ⊗(y: AbsMat): AbsMat = DecomposedMat(m.map { row => row.map(_ ⊗ y) })
    def *(y: Rep[AbsVec]): Rep[AbsVec] = ???
  }
}

trait CppCodeGen_DecompMatrix extends ExtendedCPPCodeGen {
  override def remap(m: Manifest[_]): String = {
    if (m.runtimeClass.getName.endsWith("AbsMat")) "AbsMat"
    else if (m.runtimeClass.getName.endsWith("AbsVec")) "AbsVec"
    else if (m.runtimeClass.getName.endsWith("MElem")) "AbsMat"
    else super.remap(m)
  }

  override def quote(s: Def): String = s match {
    case _                 => super.quote(s)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "kron", List(x, y, cols, rows), _) => es"$x ⊗ $y /* dim: ($cols, $rows) */"
    case n @ Node(s, "matvecprod", List(x, y), _) => es"$x * $y"
    case _                                       => super.shallow(n)
  }
}

abstract class StagedDecomposedMat extends DslDriverCPP[Int, Unit] with DecompMatrix { q =>
  override val codegen = new QCodeGen with CppCodeGen_DecompMatrix {
    val IR: q.type = q
  }

  def snippet(n: Rep[Int]): Rep[Unit] = {
    val id2 = id(2)
    val zr2 = sqZeros(2)
    val m1 = DecomposedMat(List(
      List(id(2), sqZeros(2)),
      List(sqZeros(2), id(2))))
    val m2 = rand(4, 4)
    val m3 = m1 ⊗ m2 //⊗ m1
    m3.draw()
    //m1.draw()
    println(id2(0, 0))
    //(zr2 ⊗ m2).draw
    println("End")
  }
}

object TestStagedDecompMatrix {
  def main(args: Array[String]): Unit = {
    val driver = new StagedDecomposedMat {}
    println(driver.code)
  }
}
