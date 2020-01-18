package donovan
import donovan.json.{JPath, TypesByPath}
import io.circe.Json

object examples extends App {
  import donovan.implicits._

  lazy val exampleJson: Json = hoconAsJson("""foo.bar.flag = true
      |foo.bar.array = [100,101,102,103]
      |points : [
      |  {
      |    x : 1
      |    y : 2
      |    double : [[false]]
      |  },
      |  {
      |    x : 3
      |    y : 4
      |  }
      |]
    """.stripMargin)

  object time {
    val Some(fiveMinAgo) = "5 minutes ago".asDate()

    // 10 minutes before 5 minutes ago
    val Some(fifteenMinAgo) = "10 min ago".asDate(fiveMinAgo)

    println(s"fifteen min ago is $fifteenMinAgo")
  }

  def filtering {
    println(exampleJson.spaces4)

    val justFoo: Json = exampleJson.filter(
      JPath("foo", "bar", "array", "1"),
      JPath("foo", "bar", "flag")
    )
    println(justFoo.spaces4)

    val paths: TypesByPath = justFoo.typesByPath
    println(paths.mkString("The filtered out paths are:\n", "\n", "\n"))

  }

  object matching {
    val flagIsTrue  = "foo.bar.flag" === true
    val flagIsFalse = "foo.bar.flag" === false

    println(flagIsTrue.asMatcher().matches(exampleJson))
    println(flagIsFalse.asMatcher().matches(exampleJson))
    println(flagIsFalse.asMatcher().and(flagIsFalse).matches(exampleJson))
  }

  filtering
  matching

}
