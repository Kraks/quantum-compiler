package quantum.schrodinger

import java.text.DecimalFormat
import Math.abs

case class Complex(re: Double, im: Double) {
  override def toString = s"{$re, $im}"

  def prettyPrint: String = {
    val decFormat = new DecimalFormat("#.000")
    val reStr = decFormat.format(re)
    val imStr = decFormat.format(im)
    if (im == 0) reStr
    else reStr + (if (im > 0) "+" + imStr else imStr) + "*i"
  }

  def +(c: Complex) = Complex(re + c.re, im + c.im)
  def -(c: Complex) = Complex(re - c.re, im - c.im)
  def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)

  def ≈(c: Complex): Boolean = {
    val eps = 0.001
    abs(im - c.im) <= eps && abs(re - c.re) <= eps
  }
  def !≈(c: Complex): Boolean = !this.≈(c)
}

object Complex {
  implicit def fromDouble(d: Double) = Complex(d, 0)
}
