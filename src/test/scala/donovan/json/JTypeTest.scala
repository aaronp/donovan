package donovan.json

import io.circe.Json
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JTypeTest extends AnyWordSpec with Matchers {

  "JType" should {
    List(
      JType.NullType,
      JType.BooleanType,
      JType.NumericType,
      JType.TextType,
      JType.ArrayType,
      JType.ObjType
    ).foreach { expected: JType =>
      s"$expected should be serializable from/to json" in {
        import io.circe.syntax._

        val json: Json = expected.asJson

        json.as[JType] shouldBe Right(expected)
      }
    }
  }
}
