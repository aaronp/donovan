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

      json.filter("foo.array[1].nested[0].grandchild2".asJPath) shouldBe json"""{
         |  "foo" : {
         |    "array" : [
         |      {
         |        "nested" : [ { "grandchild2" : 2 } ]
         |      }
         |    ]
         |  }
         |}"""

      json.filter("baz".asJPath, "meh".asJPath) shouldBe
        json""" {
               |    "baz" : true,
               |    "meh" : false
               |}
             """
    }
  }
}
