package donovan.json

import donovan.{BaseJsonSpec, implicits}
import io.circe.parser._

class JArrayFindTest extends BaseJsonSpec with implicits {
  "JArrayFind" should {
    "marshal to/from json" in {
      val input =
        """[
          |    "foo",
          |    {
          |        "arrayFind" : {
          |            "eq" : "x"
          |        }
          |    }
          |]""".stripMargin

      val expected: JPath = "foo".asJPath :+ Eq(json"x").inArray
      decode[JPath](input) shouldBe Right(expected)
    }
  }
}
