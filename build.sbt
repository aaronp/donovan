import sbt.Keys.{organization, publishMavenStyle, publishTo}
import scoverage.ScoverageKeys.coverageFailOnMinimum
import org.scoverage.coveralls.Imports.CoverallsKeys._
import ReleaseTransformations._

val repo = "donovan"
name := repo
val username = "aaronp"

val scalaTwelve         = "2.12.10"
val scalaThirteen       = "2.13.0"
val defaultScalaVersion = scalaThirteen
crossScalaVersions := Seq(scalaThirteen, scalaTwelve)
scalaVersion := defaultScalaVersion
organization := s"com.github.${username}"
coverallsTokenFile := Option((Path.userHome / ".sbt" / ".coveralls.donovan").asPath.toString)

// see https://github.com/sbt/sbt-ghpages
// this exposes the 'ghpagesPushSite' task
enablePlugins(GhpagesPlugin)
enablePlugins(PamfletPlugin)
enablePlugins(SiteScaladocPlugin)
enablePlugins(BuildInfoPlugin)
enablePlugins(GitVersioning)

releaseCrossBuild := true

test in assembly := {}

lazy val noPublishSettings = skip in publish := true

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

coverageMinimum := 75
coverageFailOnMinimum := true

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
autoAPIMappings := true
exportJars := false
javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-XX:MaxMetaspaceSize=1g")
git.useGitDescribe := false
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := s"${repo}.build"
assemblyMergeStrategy in assembly := {
  case str if str.contains("application.conf") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

packageOptions in (Compile, packageBin) += Package.ManifestAttributes("git-sha" -> git.gitHeadCommit.value.getOrElse("unknown"))

// see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
testOptions in Test += (Tests.Argument(TestFrameworks.ScalaTest, "-h", s"target/scalatest-reports", "-oN"))

// put scaladocs under 'api/latest'
sourceDirectory in Pamflet := sourceDirectory.value / "site"

// see https://www.scala-sbt.org/sbt-site/api-documentation.html
siteSubdirName in SiteScaladoc := "api/latest"

scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits")

git.gitTagToVersionNumber := { tag: String =>
  if (tag matches "v?[0-9]+\\..*") {
    Some(tag)
  } else None
}

// see http://scalameta.org/scalafmt/
scalafmtOnCompile in ThisBuild := true
scalafmtVersion in ThisBuild := "1.4.0"

git.remoteRepo := s"git@github.com:$username/$repo.git"
ghpagesNoJekyll := true

lazy val settings = scalafmtSettings

scalacOptions ++= List(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:reflectiveCalls", // Allow reflective calls
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions" // Allow definition of implicit functions called views
)

libraryDependencies ++= Dependencies.all

pomExtra in Global := {
  <url>https://github.com/${username}/${repo}
    </url>
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        </license>
      </licenses>
      <developers>
        <developer>
          <id>${username}</id>
          <name>Aaron Pritzlaff</name>
          <url>https://github.com/${username}/${repo}
          </url>
        </developer>
      </developers>
}
