package donovan.json
import donovan.BaseJsonSpec
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

import scala.io.StdIn

object PerformanceRunner {

  def main(a: Array[String]): Unit = {
    val default = 50000000
    val nrRuns  = StdIn.readLine(s"Number to execute (default is $default):")
    val runs = nrRuns.trim match {
      case "" => default
      case nr => nr.toInt
    }
    runExpressionPerf(5, runs)
  }

  def profile(label: String, warmUp: Int, runs: Int, eval: EvalJson) = {
    (0 to warmUp).foreach { _ =>
      time(label, runs) {
        eval.matches(aJson)
      }
    }
  }

  def runExpressionPerf(warmUp: Int, runs: Int) = {
    import donovan.implicits._
    val equalToTen = "value".equalTo(10).asMatcher()
    profile("expression", warmUp, runs, EvalJson(equalToTen))
  }

  def runEqualsPerf(warmUp: Int, runs: Int) = {
    profile("equals", warmUp, runs, EvalJson.equalTo(Json.fromInt(10)))
  }

  trait EvalJson {
    def matches(json: Json): Boolean
  }
  object EvalJson {
    def equalTo(expected: Json) = new EvalJson { override def matches(json: Json): Boolean = json == expected   }
    def apply(test: JPredicate) = new EvalJson { override def matches(json: Json): Boolean = test.matches(json) }
  }
  case class TreeNode(value: Int, children: List[TreeNode])

  val List(one, two, three) = (0 until 3).map(i => TreeNode(i, Nil)).toList
  val c                     = TreeNode(30, List(one, two, three))
  val b                     = TreeNode(20, List(c))
  val a                     = TreeNode(10, List(b, c))

  val aJson = a.asJson

  def time(label: String, count: Int, eval: EvalJson, json: Json) = {
    val nanos = timeInNanos(count)(eval.matches(json))
    import concurrent.duration._
    val pretty = ("000" + count).reverse.grouped(3).mkString(",").reverse.dropWhile(c => c == '0' || c == ',')
    println(s"$label of $pretty runs took ${nanos}ns (${nanos.nanos.toMillis}ms)")
  }
  def time(label: String, count: Int)(thunk: => Unit) = {
    val nanos = timeInNanos(count)(thunk)
    import concurrent.duration._
    val pretty = ("000" + count).reverse.grouped(3).mkString(",").reverse.dropWhile(c => c == '0' || c == ',')
    println(s"$label of $pretty runs took ${nanos}ns (${nanos.nanos.toMillis}ms)")
  }
  def timeInNanos(count: Int)(thunk: => Unit): Long = {
    val before = System.nanoTime()
    require(count > 0)
    var i = count
    while (i > 0) {
      thunk
      i = i - 1
    }
    System.nanoTime() - before
  }
}
