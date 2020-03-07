package donovan.json

import io.circe.generic.auto._
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ValuesByPathTest extends AnyWordSpec with Matchers with GivenWhenThen {

  import ValuesByPathTest._

  "ValuesByPath" should {
    "the json values by path" in {
      import io.circe.syntax._
      val json = ComplexType().asJson

      val expected = Map(
        ("children.[0].id", "first".asJson),
        ("children.[0].values.[0]", "num 0".asJson.asJson),
        ("children.[0].values.[1]", "num 1".asJson),
        ("children.[0].values.[2]", "num 2".asJson),
        ("children.[0].values.[3]", "num 3".asJson),
        ("children.[1].id", "second".asJson),
        ("children.[1].values.[0]", "x1000".asJson),
        ("children.[1].values.[1]", "x1001".asJson),
        ("id", "foo".asJson),
        ("values.[0]", "value 0".asJson),
        ("values.[1]", "value 1".asJson),
        ("values.[2]", "value 2".asJson),
        ("values.[3]", "value 3".asJson)
      )

      val actual = ValuesByPath(json).map {
        case (key, value) => key.mkString(".") -> value
      }
      withClue(actual.toSeq.sortBy(_._1).mkString("\n")) {
        actual shouldBe expected
      }

    }
  }
}

object ValuesByPathTest {

  case class Child(id: String, values: Seq[String])

  case class ComplexType(id: String = "foo",
                         values: Seq[String] = (0 to 3).map("value " + _),
                         children: Seq[Child] = Seq(Child("first", (0 to 3).map("num " + _)), Child("second", (1000 to 1001).map("x" + _))))

}
