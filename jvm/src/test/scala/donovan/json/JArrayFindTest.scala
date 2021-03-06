package donovan.json

import donovan.{BaseJsonSpec, implicits}
import io.circe.parser._

class JArrayFindTest extends BaseJsonSpec with implicits {
  "JArrayFind" should {
    "marshal to/from json" in {
      val input =
        """{
            "select" : [
              {
                "arrayFind" : {
                  "select" : [
                    {
                      "field" : "meh",
                      "predicate" : {
                        "eq" : "x"
                      }
                    }
                  ],
                  "test" : "match-all"
                }
              }
            ],
            "test" : "match-all"
          }"""

      val expected = JArrayFind("meh" === "x").asMatcher()
      decode[JPredicate](input) shouldBe Right(expected)
    }
  }
}
