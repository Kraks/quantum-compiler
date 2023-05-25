package quantum.tensor

case class Complex(re: Double, im: Double) {
  override def toString = s"{$re, $im}"
  def +(c: Complex) = Complex(re + c.re, im + c.im)
  def -(c: Complex) = Complex(re - c.re, im - c.im)
  def *(c: Complex) = Complex(re * c.re - im * c.im, re * c.im + im * c.re)
}

object Complex {
  implicit def fromDouble(d: Double) = Complex(d, 0)
}
