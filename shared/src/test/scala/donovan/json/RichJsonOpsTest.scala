package donovan.json
import donovan.BaseJsonSpec
import io.circe.Json

class RichJsonOpsTest extends BaseJsonSpec {

  "RichJsonOps.filter" should {
    "filter out any json values which don't match the path" in {
      val json = json""" {
      "foo" : {
        "bar" : 1,
        "array" : [
          { "nested" : [] },
          {
            "nested" : [
               {
                 "grandchild1" : 1,
                 "grandchild2" : 2
               },
               {
                 "grandchild3" : 3,
                 "grandchild4" : 4
               }
            ],
            "flag" : false
          }
        ]
      },
      "baz" : true,
      "meh" : false
      }"""

      import donovan.implicits._
      val p2 = "foo.array[1].nested".asJPath
      println(p2)
      val result: Json = json.filter("baz".asJPath, p2)

      println(result.spaces4)
      println(result.paths.mkString("\n"))
    }
  }
}
