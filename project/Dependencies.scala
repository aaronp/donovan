import sbt._
object Dependencies {

  val config = "com.typesafe" % "config" % "1.3.0" % "test"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2" % "provided"

  val testDependencies = List(
    "org.scalactic" %% "scalactic" % "3.1.0" % "test",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % "test",
    "junit" % "junit" % "4.13" % "test"
  )

  val circe = List("core", "generic", "parser").map(name => "io.circe" %% s"circe-$name" % "0.12.3")


  // https://github.com/vmunier/play-scalajs.g8/issues/20
  val JVM = Def.setting(config :: testDependencies ::: circe)
  val Javascript = circe
}
