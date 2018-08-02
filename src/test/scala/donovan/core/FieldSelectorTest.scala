package donovan.core

import donovan.{BaseJsonSpec, implicits}
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

class FieldSelectorTest extends BaseJsonSpec with implicits {

  import FieldSelectorTest._

  "FieldSelector instances" should {
    "be able to get fields from json" in {
      val selector = donovan.json.jsonSelectorForPath("y".asJPath)
      selector.select(MyData(None, 42).asJson) shouldBe Json.fromInt(42)
    }
    "be able to get fields from some other data type" in {
      val selector = FieldSelector.lift[MyData, Int](_.y)
      selector.select(MyData(None, 3)) shouldBe 3
    }
  }
}

object FieldSelectorTest {

  case class MyData(x: Option[Int], y: Int)

}
