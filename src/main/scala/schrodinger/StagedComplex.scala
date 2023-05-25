package quantum.schrodinger.staged

import math.pow
import quantum._

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import lms.core.Backend._
import quantum.schrodinger.Complex

// Staged complex numbers

trait ComplexOps { b: Dsl =>
  object Complex {
    def apply(re: Rep[Double], im: Rep[Double]): Rep[Complex] =
      Wrap[Complex](Adapter.g.reflect("complex-new", Unwrap(re), Unwrap(im)))
  }

  implicit def liftComplex(c: Complex): Rep[Complex]       = Complex(c.re, c.im)
  implicit def liftDouble(d: Double): Rep[Complex]         = Complex(d, 0)
  implicit def liftRepDouble(d: Rep[Double]): Rep[Complex] = Complex(d, 0)

  implicit class ComplexOps(c: Rep[Complex]) {
    def re: Rep[Double]                  = Wrap[Double](Adapter.g.reflect("complex-re", Unwrap(c)))
    def im: Rep[Double]                  = Wrap[Double](Adapter.g.reflect("complex-im", Unwrap(c)))
    def +(d: Rep[Complex]): Rep[Complex] = Complex(c.re + d.re, c.im + d.im)
    def +(d: Complex): Rep[Complex]      = Complex(c.re + d.re, c.im + d.im)
    def -(d: Rep[Complex]): Rep[Complex] = Complex(c.re - d.re, c.im - d.im)
    def -(d: Complex): Rep[Complex]      = Complex(c.re - d.re, c.im - d.im)
    def *(d: Rep[Complex]): Rep[Complex] = Complex(c.re * d.re - c.im * d.im, c.re * d.im + c.im * d.re)
    def *(d: Complex): Rep[Complex]      = Complex(c.re * d.re - c.im * d.im, c.re * d.im + c.im * d.re)
  }

  implicit class StaticComplexOps(c: Complex) {
    def +(d: Rep[Complex]): Rep[Complex] = Complex(c.re + d.re, c.im + d.im)
    def -(d: Rep[Complex]): Rep[Complex] = Complex(c.re - d.re, c.im - d.im)
    def *(d: Rep[Complex]): Rep[Complex] = Complex(c.re * d.re - c.im * d.im, c.re * d.im + c.im * d.re)
  }
}

trait CppCodeGen_Complex extends ExtendedCPPCodeGen {
  override def remap(m: Manifest[_]): String = {
    System.out.println(m.runtimeClass.getName)
    if (m.runtimeClass.getName.endsWith("Complex")) "Complex"
    else super.remap(m)
  }

  override def quote(s: Def): String = s match {
    case Const(c: Complex) => s"{ ${c.re}, ${c.im} }"
    case _                 => super.quote(s)
  }

  override def shallow(n: Node): Unit = n match {
    case Node(s, "complex-new", List(re, im), _) => es"{ ${re}, ${im} }"
    case Node(s, "complex-re", List(c), _)       => es"$c.re"
    case Node(s, "complex-im", List(c), _)       => es"$c.im"
    case _                                       => super.shallow(n)
  }
}
