package donovan.json

import donovan.BaseJsonSpec
import donovan.implicits._
import io.circe.Json

class RichJsonOpsTest extends BaseJsonSpec {

  "RichJsonOps.anonymize" should {
    "replace the json values" in {

      val original =
        json""" {
           "user" : "david",
           "id" : false
            }"""

      original.anonymize shouldBe
        json"""{
          "id" : true,
          "user" : "text"
        }"""

    }
  }
  "RichJsonOps.filter" should {
    "filter out any json values which don't match the path" in {
      val original =
        json""" {
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

      val p2 = "foo.array[1].nested".asJPath
      p2 shouldBe JPath(List(JField("foo"), JField("array"), JPos(1), JField("nested")))

      original.filter("baz".asJPath) shouldBe
        json"""{
            "baz" : true
        }"""

      original.filter("baz".asJPath, p2) shouldBe json"""{
                "foo" : {
                    "array" : [
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
                            ]
                        }
                    ]
                },
                "baz" : true
            }"""

    }
  }
}
