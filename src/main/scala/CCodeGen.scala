package quantum.schrodinger.staged

import lms.core._
import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext
import lms.thirdparty.CLibs
import lms.thirdparty.CCodeGenLibs
import lms.core.Backend._
import java.io.{ByteArrayOutputStream, PrintStream}

// Extends LMS C++ code generator with staticData

abstract class QCodeGen extends DslGenCPP {
  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "staticData", List(Backend.Const(a)), _) =>
      val q = a match {
        case x: Array[_] => "Array(" + x.mkString(",") + ")"
        case _           => a
      }
      emit("p" + quote(s)); emit(s" /* staticData $q */")
    case n =>
      super.shallow(n)
  }

  // Note: so far only handles scalar values and flat arrays
  override def quoteStatic(n: Node) = n match {
    case Node(s, "staticData", List(Backend.Const(a)), _) =>
      val arg = "p" + quote(s)
      val m   = typeMap.getOrElse(s, manifest[Unknown])
      val (tpe, postfix) = m.typeArguments match {
        case Nil         => (remap(m), "")
        case List(inner) => (remap(inner), "[]")
      }
      val rhs = m.typeArguments match {
        case Nil         => a.toString
        case List(inner) => "{" + a.asInstanceOf[Array[_]].mkString(",") + "}"
      }
      s"$tpe $arg$postfix = $rhs;"
  }

  def emitStatics(out: PrintStream): Unit = dce.statics.foreach { n => out.println(quoteStatic(n)) }

  registerHeader("<iostream>")
  registerHeader("<chrono>")
  registerHeader("<cstring>")

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

  val initInput: String  = "int input[] = {1, 2, 3, 4, 5};"
  val procOutput: String = "printArray(output, 5);";
  def declareOutput(m: Manifest[_]): String = {
    if (remap(m) == "void") ""
    else s"${remap(m)} output = "
  }

  override def emitAll(g: Graph, name: String)(m1: Manifest[_], m2: Manifest[_]): Unit = {
    val ng  = init(g)
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
    |  $initInput
    |  auto start = high_resolution_clock::now();
    |  ${declareOutput(m2)}$name(input);
    |  auto end = high_resolution_clock::now();
    |  $procOutput
    |  auto duration = duration_cast<microseconds>(end - start);
    |  std::cout << std::fixed;
    |  std::cout << "time: ";
    |  std::cout << (duration_cast<microseconds>(duration).count() / 1.0e6) << "s\\n";
    |  return 0;
    |}""".stripMargin)
  }
}
