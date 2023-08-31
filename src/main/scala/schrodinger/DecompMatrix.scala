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
  abstract class SmallMatrix

  def zeros(cols: Int, rows: Int): Rep[SmallMatrix] =
    Wrap[SmallMatrix](Adapter.g.reflectWrite("zeros", Unwrap(unit(cols)), Unwrap(unit(rows)))(Adapter.CTRL))
  def sqZeros(n: Int): Rep[SmallMatrix] =
    Wrap[SmallMatrix](Adapter.g.reflectWrite("sq-zeros", Unwrap(unit(n)))(Adapter.CTRL))
  def id(n: Int): Rep[SmallMatrix] =
    Wrap[SmallMatrix](Adapter.g.reflectWrite("id", Unwrap(unit(n)))(Adapter.CTRL))
  def invId(n: Int): Rep[SmallMatrix] =
    Wrap[SmallMatrix](Adapter.g.reflectWrite("inv-id", Unwrap(unit(n)))(Adapter.CTRL))

  def dim(m: Rep[SmallMatrix]): (Int, Int) /* (cols, rows) */ = m match {
    case Adapter.g.Def("zeros", SList(Backend.Const(cols: Int), Backend.Const(rows: Int))) => (cols, rows)
    case Adapter.g.Def("sq-zeros", SList(Backend.Const(n: Int))) => (n, n)
    case Adapter.g.Def("id", SList(Backend.Const(n: Int))) => (n, n)
    case Adapter.g.Def("inv-id", SList(Backend.Const(n: Int))) => (n, n)
    case Adapter.g.Def("kron", SList(_, _, Backend.Const(cols: Int), Backend.Const(rows: Int))) =>
      (cols, rows)
  }

  implicit class AbsMatOps(x: Rep[SmallMatrix]) {
    def ⊗(y: Rep[SmallMatrix]): DecompMatrix = x match {
      case Adapter.g.Def("sq-zeros", SList(Backend.Const(h: Int))) =>
        val (m, n) = dim(y)
        DecompMatrix(List(List(zeros(h*m, h*n))))
      case Adapter.g.Def("id", SList(Backend.Const(h: Int))) =>
        val (m, n) = dim(y)
        val zs = List.fill(h)(List.fill(h)(zeros(m, n)))
        DecompMatrix(zs.zipWithIndex.map { case (z, i) => z.updated(i, y) })
      case _ =>
        val (m, n) = dim(x)
        val (p, q) = dim(y)
        val mat = Wrap[SmallMatrix](Adapter.g.reflectWrite("kron", Unwrap(x), Unwrap(y), Unwrap(unit(m*p)), Unwrap(unit(q*n)))(Adapter.CTRL))
        DecompMatrix(List(List(mat)))
    }
    //def *(y: Rep[AbsVec]): Rep[AbsVec] =
    //  Wrap[AbsVec](Adapter.g.reflectWrite("matvecprod", Unwrap(x), Unwrap(y))(Adapter.CTRL))
  }

  // Note: could be list[list[decompmatrix]]?
  case class DecompMatrix(m: List[List[Rep[SmallMatrix]]]) {
    def ⊗(y: DecompMatrix): DecompMatrix = ???
  }
}


