import sbt._

object Dependencies {

  val config = "com.typesafe" % "config" % "1.3.0"

  //https://github.com/typesafehub/scala-logging
  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"

  val testDependencies = List(
    "org.scalactic" %% "scalactic" % "3.0.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % "test",
    "junit" % "junit" % "4.12" % "test"
  )

  val circe: List[ModuleID] = List("core", "generic", "parser", "optics", "java8").map(name => "io.circe" %% s"circe-$name" % "0.9.1")

  val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.12.0"
  val Json: List[ModuleID] = simulacrum :: config :: circe ::: testDependencies
}
