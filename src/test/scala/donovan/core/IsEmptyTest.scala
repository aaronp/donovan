package donovan.core
import donovan.BaseJsonSpec

class IsEmptyTest extends BaseJsonSpec {

  "IsEmpty" should {
    "work for json" in {
      import IsEmpty.ops._
      json"{}".isEmpty shouldBe true
      json"{}".nonEmpty shouldBe false
    }
    "work for arrays" in {
      def requiresIsEmpty[T: IsEmpty](value: T) = {
        import IsEmpty.ops._
        value.isEmpty
      }

      requiresIsEmpty(null: String) shouldBe true
      requiresIsEmpty("") shouldBe true
      requiresIsEmpty("hi") shouldBe false
      requiresIsEmpty(Seq[Int]()) shouldBe true
      requiresIsEmpty(Array[Int]()) shouldBe true

    }
  }
}
