package donovan
import donovan.json.{JPath, TypesByPath}
import io.circe.Json

object examples extends App {
  import donovan.implicits._

  lazy val exampleJson: Json = hoconAsJson("""foo.bar.flag = true
      |foo.bar.array = [100,101,102,103]
      |foo.timestamp = "2019-03-05T12:13:14"
      |points : [
      |  {
      |    x : 1
      |    y : 2
      |    double : [[false]]
      |  },
      |  {
      |    x : 3
      |    y : 4
      |    time : "2019-03-05T00.00.00"
      |  }
      |]
    """.stripMargin)

  object time {
    val Some(fiveMinAgo) = "5 minutes ago".asDate()
    println(s"fifteen min ago is $fiveMinAgo")

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
    val flagIsTrue          = "foo.bar.flag".asJPath === true
    val timestampBeforeTime = "foo.timestamp".asJPath isBefore "points.1.time".asJPath

    println("json equals check is")
    println(flagIsTrue.json)

    println("compare check is")
    println(timestampBeforeTime.json)

    println("both is")
    println(timestampBeforeTime.and(flagIsTrue).json)

    println("flag is true:" + flagIsTrue.asMatcher().matches(exampleJson))
    println("timestampBeforeTime:" + timestampBeforeTime.matches(exampleJson))
    println("timestampBeforeTime ang flag:" + timestampBeforeTime.and(flagIsTrue).matches(exampleJson))
  }

  filtering
  matching

}
