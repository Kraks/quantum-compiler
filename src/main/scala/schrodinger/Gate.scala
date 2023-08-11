package quantum.schrodinger.gate

import math.{pow, log}
import quantum.schrodinger.Complex
import quantum.schrodinger.Complex._
import quantum.schrodinger.Matrix._

// Static gate matrix-representations

case class Gate(id: String, m: Matrix) {
  def arity: Int = (log(m.size) / log(2)).toInt
}

object Gate {
  val isq2 = 1.0 / pow(2.0, 0.5)

  val H = Gate(
    "H",
    Array(
      Array(isq2, isq2),
      Array(isq2, -isq2)
    )
  )

  val SWAP = Gate(
    "SWAP",
    Array(
      Array(1, 0, 0, 0),
      Array(0, 0, 1, 0),
      Array(0, 1, 0, 0),
      Array(0, 0, 0, 1)
    )
  )

  val NOT = Gate(
    "NOT",
    Array(
      Array(0, 1),
      Array(1, 0)
    )
  )

  val CNOT = Gate(
    "CNOT",
    Array(
      Array(1, 0, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 0, 0, 1),
      Array(0, 0, 1, 0)
    )
  )

  val S = Gate(
    "S",
    Array(
      Array(1, 0),
      Array(0, Complex(0, 1))
    )
  )

  val T = Gate(
    "T",
    Array(
      Array(1, 0),
      Array(0, isq2 + isq2 * Complex(0, 1))
    )
  )

  val Z = Gate(
    "Z",
    Array(
      Array(1, 0),
      Array(0, -1)
    )
  )

  val P = Gate(
    "P",
    0.5 * Z.m
  )

  val CZ = Gate(
    "CZ",
    Array(
      Array(1, 0, 0, 0),
      Array(0, 1, 0, 0),
      Array(0, 0, 1, 0),
      Array(0, 0, 0, -1)
    )
  )

  val CCNOT = Gate(
    "CCNOT",
    Array(
      Array(1, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 1, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 1, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1),
      Array(0, 0, 0, 0, 0, 0, 1, 0)
    )
  )
}
