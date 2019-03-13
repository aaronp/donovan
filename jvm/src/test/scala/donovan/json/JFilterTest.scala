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
      val jfilterJson = json"""["x", { "gte" : 5 } ]"""

      val expected: JPath = "x".asJPath gte 5

      jfilterJson.as[JPath] shouldBe Right(expected)
    }
    "pickle lt to/from json" in {
      val jfilterJson =
        json"""[
              |    "someField",
              |    { "lt" : 5 }
              |]"""

      val expected = "someField".asJPath lt 5
      jfilterJson.as[JPath] shouldBe Right(expected)
    }
  }
}
