import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._
object Dependencies {

  val config = "com.typesafe" % "config" % "1.3.0" % "test"

  //https://github.com/typesafehub/scala-logging
  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"

  val testDependencies = List(
    "org.scalactic" %% "scalactic" % "3.0.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % "test",
    "junit" % "junit" % "4.12" % "test"
  )

  val circe = {
    Def.setting{
      ("com.github.mpilquist" %%% "simulacrum" % "0.12.0") ::
      List("core", "generic", "parser", "java8").map(name => "io.circe" %% s"circe-$name" % "0.11.1") ++
      List("optics").map(name => "io.circe" %% s"circe-$name" % "0.11.0")
    }
  }


  // https://github.com/vmunier/play-scalajs.g8/issues/20
  val JVM = Def.setting(config :: testDependencies ::: circe.value)
  val Javascript = circe
}
