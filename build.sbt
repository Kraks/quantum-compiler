name                := "quantum-lms-compiler"
organization        := "edu.purdue"
scalaVersion        := "2.12.10"
version             := "0.1.0-SNAPSHOT"
autoCompilerPlugins := true

val paradiseVersion = "2.1.0"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.3"
libraryDependencies += "org.scalatest"          %% "scalatest"                   % "3.2.9" % Test

addCompilerPlugin("org.typelevel"  %% "kind-projector" % "0.10.3")
addCompilerPlugin("org.scalamacros" % "paradise"       % paradiseVersion cross CrossVersion.full)
addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "1.0.3")
parallelExecution in Test                 := false

scalacOptions += "-P:continuations:enable"

lazy val lms = ProjectRef(file("./lms-clean"), "lms-clean")

lazy val root = (project in file(".")).dependsOn(lms % "test->test; compile->compile")
