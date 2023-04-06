package quantum.tensor

import math.pow

case class Complex(re: Double, im: Double) {
  override def toString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
  def +(c: Complex) = Complex(re + c.re, im + c.im)
  def -(c: Complex) = Complex(re - c.re, im - c.im)
  def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)
}

object Complex {
  implicit def fromDouble(d: Double) = Complex(d, 0)
}

import Complex._

case class Matrix(m: Array[Array[Complex]]) {
  def ⊗(m1: Matrix): Matrix = ???
  def *(m1: Matrix): Matrix = ???
  def *(m1: Array[Complex]): Array[Complex] = ???
  def arity: Int = pow(m.size, 0.5).toInt
}

object Matrix {
  def identity(n: Int): Matrix = ???
  def zeros(n: Int): Matrix = ???
}

class QState(var s: Array[Complex], size: Int) {
  def op(g: Matrix, i: Int) = {
    val iLeft = Matrix.identity(pow(i, 2).toInt)
    val iRight = Matrix.identity(size - i - g.arity)
    s = ((iLeft ⊗ g) ⊗ iRight) * s
  }
}

object QState {
  def apply(n: Int): QState = {
    val s = new Array[Complex](pow(n, 2).toInt)
    s(0) = 1
    new QState(s, n)
  }
}
