package shonan

// The Shonan Challenge - matrix vector product
// https://scala-lms.github.io/tutorials/shonan.html

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs

import lms.core.Backend._
import java.io.{ByteArrayOutputStream, PrintStream}

import quantum.QCodeGen

object Shonan {
  val A = scala.Array
  val a =
    A(A(1, 1, 1, 1, 1), 
      A(0, 0, 0, 0, 0), 
      A(0, 0, 1, 0, 0), 
      A(0, 0, 0, 0, 0),
      A(0, 0, 1, 0, 1))

  val snippet = new DslDriverCPP[Array[Int], Array[Int]] { q =>
    override val codegen = new QCodeGen {
      val IR: q.type = q
    }

    def unrollIf(c: Boolean, r: Range) = new {
      def foreach(f: Rep[Int] => Rep[Unit]) = {
        if (c) for (j <- (r.start until r.end): Range) f(j)
        else for (j <- (r.start until r.end): Rep[Range]) f(j)
      }
    }

    def snippet(v: Rep[Array[Int]]) = {
      def matVecProd(a0: Array[Array[Int]], v: Rep[Array[Int]]): Rep[Array[Int]] = {
        val n = a0.length
        val a = staticData(a0)
        val v1 = NewArray[Int](n)

        for (i <- (0 until n): Range) {
          val sparse = a0(i).count(_ != 0) < 3
          for (j <- unrollIf(sparse, 0 until n)) {
            v1(i) = v1(i) + a(i).apply(j) * v(j)
          }
        }
        v1
      }
      matVecProd(a, v)
    }
  }

  def main(args: Array[String]): Unit = {
    println(snippet.code)
    snippet.eval(Array(1,2,3,4,5))
    //assert(snippet.eval(Array(1,2,3,4,5)).toList == List(15, 0, 3, 0, 8))
  }
}
