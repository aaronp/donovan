package donovan
import donovan.json.{JPath, TypesByPath}
import io.circe.Json
import donovan.implicits._

/**
  * Just a place to consolidate examples which otherwise can be found in tests
  */
object examples extends App {

  lazy val exampleJson: Json = hoconAsJson("""
      |foo.flag : true
      |foo.bar.array : [100,101,102,103]
      |foo.timestamp : "2019-03-05T12:13:14"
      |foo.relative1  : "10 minutes ago"
      |foo.relative2  : "10 seconds ago"
      |
      |points : [
      |  {
      |    x : 1
      |    y : 2
      |    double : [[false]]
      |  },
      |  {
      |    x : 3
      |    y : 4
      |    time : "2019-03-05T00:00:00"
      |  }
      |]
    """.stripMargin)

  def section(header: String)(thunk: => Unit) = {
    println(s"\n\n====================== $header ======================")
    thunk
  }

  def time = section("time") {
    val Some(fiveMinAgo) = "5 minutes ago".asDate()
    println(s"five min ago is $fiveMinAgo")

    // 10 minutes before 5 minutes ago
    val Some(fifteenMinAgo) = "10 min ago".asDate(fiveMinAgo)
    println(s"fifteen min ago is $fifteenMinAgo")

    val Some(inFourMinutes) = "4 min".asDate()
    println(s"4 minutes from now is $inFourMinutes")
  }

  def filtering = section("filtering") {
    println(exampleJson.spaces4)

    // filter some json, only keeping the specified paths:
    val filtered: Json = exampleJson.filter(
      "foo.bar.array.1".asJPath,
      "foo.flag".asJPath
    )
    println("Only keeping certain parts of the json:")
    println(filtered.spaces4)
  }

  def types = section("types") {
    val paths: TypesByPath = exampleJson.typesByPath
    println(paths.mkString("The paths/types from our json are:\n", "\n", "\n"))
  }

  def appending = section("appending") {
    // append a new value in json
    val helloWorld        = Json.obj("hello" -> Json.fromString("world"))
    val Some(addedHello)  = "points.0".asJPath.appendTo(exampleJson, helloWorld)
    val Some(addedHello2) = JPath.root.appendTo(exampleJson, helloWorld)

    println()
    println(addedHello.spaces4)
    println()
    println(addedHello2.spaces4)
    println()
  }
  def computingValues = section("computing") {
    // some two integer values in the document ....
    val addPoints = "points.0.x".asExpression + "points.1.x".asExpression

    val Some(sum) = addPoints.eval(exampleJson)
    println(s"points.0.x + points.1.x is $sum")

    val concatPoints = "foo.relative1".asExpression concat "foo.relative2".asExpression
    println(s"foo.relative1 concat foo.relative2 is ${concatPoints.eval(exampleJson)}")
  }
  def matching = section("matching") {
    val flagIsTrue          = "foo.bar.flag".asJPath === true
    val timestampBeforeTime = "foo.timestamp".asJPath isBefore "points.1.time".asJPath

    println("json equals check is")
    println(flagIsTrue.json)

    println("matching values within the same json document:")
    println(timestampBeforeTime.json)

    println("both is")
    println(timestampBeforeTime.and(flagIsTrue).json)

    println("flag is true:" + flagIsTrue.asMatcher().matches(exampleJson))
    println("timestampBeforeTime:" + timestampBeforeTime.matches(exampleJson))
    println("timestampBeforeTime ang flag:" + timestampBeforeTime.and(flagIsTrue).matches(exampleJson))
  }

  types
  time
  filtering
  matching
  computingValues
  appending
}
