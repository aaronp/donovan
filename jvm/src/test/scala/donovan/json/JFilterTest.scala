package donovan.json

import donovan.{BaseJsonSpec, examples, implicits}

class JFilterTest extends BaseJsonSpec with implicits {
  "JFilter" should {
    "match using '==='" in {

      val flagIsTrue  = "foo.bar.flag".asJPath === true
      val flagIsFalse = "foo.bar.flag".asJPath === false

      flagIsTrue.matches(examples.exampleJson) shouldBe true
      flagIsFalse.matches(examples.exampleJson) shouldBe false
    }
    "pickle gte to/from json" in {
      val jfilterJson =
        json"""{
                "field" : "x",
                "predicate" : {
                  "gte" : 5
                }
                }"""

      val Some(expected) = ("x".asJPath gte 5).path.last.asFilter
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

      val expected = ("someField".asJPath gte 5).path.last

      jfilterJson.as[JPart] shouldBe Right(expected)
    }
  }
}
