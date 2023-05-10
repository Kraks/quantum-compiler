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

object Shonan {
  val A = scala.Array
  val a =
    A(A(1, 1, 1, 1, 1), 
      A(0, 0, 0, 0, 0), 
      A(0, 0, 1, 0, 0), 
      A(0, 0, 0, 0, 0),
      A(0, 0, 1, 0, 1))

  val snippet = new DslDriverCPP[Array[Int], Array[Int]] { q =>
    override val codegen = new DslGenCPP {
      val IR: q.type = q
      override def shallow(n: Node): Unit = n match {
        case n @ Node(s,"staticData",List(Backend.Const(a)),_) =>
          val q = a match {
            case x: Array[_] => "Array("+x.mkString(",")+")"
            case _ => a
          }
          emit("p"+quote(s)); emit(s" /* staticData $q */")
        case n =>
          super.shallow(n)
      }

      // Note: so far only handles scalar values and flat arrays
      override def quoteStatic(n: Node) = n match {
        case Node(s, "staticData", List(Backend.Const(a)), _) =>
          val arg = "p"+quote(s)
          val m = typeMap.getOrElse(s, manifest[Unknown])
          val (tpe, postfix) = m.typeArguments match {
            case Nil => (remap(m), "")
            case List(inner) => (remap(inner), "[]")
          }
          val rhs = m.typeArguments match {
            case Nil => a.toString
            case List(inner) => "{" + a.asInstanceOf[Array[_]].mkString(",") + "}"
          }
          s"$tpe $arg$postfix = $rhs;"
      }

      def emitStatics(out: PrintStream): Unit = dce.statics.foreach { n => out.print(quoteStatic(n)) }

      registerHeader("<iostream>")
      registerHeader("<chrono>")
      lazy val prelude = """
      |using namespace std::chrono;
      |void printArray(int arr[], int size) {
      |  printf("[");
      |  for (int i = 0; i < size; i++) {
      |    printf("%d", arr[i]);
      |    if (i < size - 1) { printf(", "); }
      |  }
      |  printf("]\n");
      |}
      """.stripMargin
      override def emitAll(g: Graph, name: String)(m1:Manifest[_],m2:Manifest[_]): Unit = {
        val ng = init(g)
        val efs = ""
        val src = run(name, ng)
        emitDefines(stream)
        emitHeaders(stream)
        emit(prelude)
        emitStatics(stream)
        emitFunctionDecls(stream)
        emitDatastructures(stream)
        emitFunctions(stream)
        emitInit(stream)
        emitln(s"\n/**************** $name ****************/")
        emit(src)
        emitln(s"""
        |int main(int argc, char *argv[]) {
        |  int input[] = {1, 2, 3, 4, 5};
        |  auto start = high_resolution_clock::now();
        |  int* output = $name(input);
        |  auto end = high_resolution_clock::now();
        |  printArray(output, 5);
        |  auto duration = duration_cast<microseconds>(end - start);
        |  std::cout << std::fixed;
        |  std::cout << "time: ";
        |  std::cout << (duration_cast<microseconds>(duration).count() / 1.0e6) << "s\\n";
        |  return 0;
        |}""".stripMargin)
      }
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
