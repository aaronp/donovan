package donovan.core
import donovan.BaseJsonSpec

class IsEmptyTest extends BaseJsonSpec {

  "IsEmpty" should {

    "work for String/Arrays/Seq" in {
      def requiresIsEmpty[T: IsEmpty](value: T) = IsEmpty[T].isEmpty(value)

      requiresIsEmpty(null: String) shouldBe true
      requiresIsEmpty("") shouldBe true
      requiresIsEmpty("hi") shouldBe false
      requiresIsEmpty(Seq[Int]()) shouldBe true
      requiresIsEmpty(Array[Int]()) shouldBe true

    }
  }
}
