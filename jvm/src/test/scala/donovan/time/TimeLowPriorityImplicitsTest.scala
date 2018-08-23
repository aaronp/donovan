package donovan.time
import donovan.BaseJsonSpec

class TimeLowPriorityImplicitsTest extends BaseJsonSpec with TimeLowPriorityImplicits {

  "TimeLowPriorityImplicits.asDate" should {
    "pimp strings w/ asDate" in {
      val Some(before) = "yesterday".asDate()
      val Some(now)    = "now".asDate()
      val Some(after)  = "tomorrow".asDate()
      now.isBefore(after) shouldBe true
      now.isAfter(before) shouldBe true

    }
  }
}
