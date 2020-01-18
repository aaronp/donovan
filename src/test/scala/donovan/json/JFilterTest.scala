package donovan.json

import donovan.{BaseJsonSpec, examples, implicits}

class JFilterTest extends BaseJsonSpec with implicits {
  "JFilter" should {
    "match using '==='" in {

      val flagIsTrue  = "foo.bar.flag" === true
      val flagIsFalse = "foo.bar.flag" === false

      flagIsTrue.asMatcher().matches(examples.exampleJson) shouldBe true
      flagIsFalse.asMatcher().matches(examples.exampleJson) shouldBe false
    }
    "pickle gte to/from json" in {
      val jfilterJson =
        json"""{
                "field" : "x",
                "predicate" : {
                  "gte" : 5
                }
                }"""

      val Some(expected) = ("x" gte 5).path.last.asFilter
      val actual         = jfilterJson.as[JPart]
      actual shouldBe Right(expected)
    }
    "pickle to/from json" in {
      val jfilterJson =
        json"""{
                  "field" : "someField",
                  "predicate" : {
                    "select" : [
                        {
                          "field" : "x",
                          "predicate" : {
                            "gte" : 5
                          }
                        }
                      ],
                    "test" : "match-all"
                  }
                }"""

      val pred            = "x" gte 5
      val expected: JPart = JFilter("someField", pred)

      jfilterJson.as[JPart] shouldBe Right(expected)
    }
  }
}
