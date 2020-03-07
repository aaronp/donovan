import sbt._
object Dependencies {

  val config = "com.typesafe" % "config" % "1.4.0" % "test"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2" % "provided"

  val testDependencies = List(
    "org.scalactic" %% "scalactic" % "3.1.1" % "test",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % "test",
    "junit" % "junit" % "4.13" % "test"
  )

  val circe = {
    val ge = "io.circe" %% "circe-generic-extras" % "0.13.0"

    ge :: List("core", "generic", "parser").map(name => "io.circe" %% s"circe-$name" % "0.13.0")
  }

  val flexmark = "com.vladsch.flexmark" % "flexmark-all" % "0.35.10" % Test

  // https://github.com/vmunier/play-scalajs.g8/issues/20
  val JVM = flexmark :: config :: testDependencies ::: circe
  val Javascript = circe
}
