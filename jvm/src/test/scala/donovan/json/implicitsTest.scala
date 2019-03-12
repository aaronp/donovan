package donovan.json

import donovan.{BaseJsonSpec, implicits}
import io.circe.Json

import scala.util.{Failure, Success, Try}

class implicitsTest extends BaseJsonSpec with implicits {

  import io.circe.generic.auto._
  import io.circe.parser._
  import io.circe.syntax._

  "implicits" should {
    "converts integers to Json.int" in {
      val value: Json = 5
      value shouldBe Json.fromInt(5)
    }
    "converts strings to Json.strings" in {
      val value: Json = "hi"
      value shouldBe Json.fromString("hi")
    }
    "be able to encode/decode Try success values" in {
      val tri: Try[Int] = Success(123)
      val json          = tri.asJson
      decode[Try[Int]](json.noSpaces) shouldBe Right(tri)
    }
    "be able to encode/decode Try failure values" in {
      val tri: Try[Int]       = Failure(new Exception("bang"))
      val json                = tri.asJson
      val Right(Failure(err)) = decode[Try[Int]](json.noSpaces)
      err.getMessage shouldBe "bang"
    }
  }
}
