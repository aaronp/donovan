package donovan.json
import donovan.BaseJsonSpec

class ComparePredicateTest extends BaseJsonSpec {

  "ComparePredicate.before" should {
    "evaluate against another value within the document" in {
      val xMatchesY = ComparePredicate(JPath("foo", "bar"), JPath("y"), Op.Before)

      xMatchesY.matches(hocon"""
             foo.bar : "10 minutes ago"
             y : "11 minutes ago"
           """) shouldBe false

      xMatchesY.matches(hocon"""
             foo.bar : "10 minutes ago"
             y : "10 minutes ago"
           """) shouldBe false

      xMatchesY.matches(hocon"""
             foo.bar : "11 minutes ago"
             y : "10 minutes ago"
           """) shouldBe true
    }
  }
  "ComparePredicate.after" should {
    "evaluate against another value within the document" in {
      val xMatchesY = ComparePredicate(JPath("foo", "bar"), JPath("y"), Op.After)

      xMatchesY.matches(hocon"""
             foo.bar : "10 minutes ago"
             y : "11 minutes ago"
           """) shouldBe true

      xMatchesY.matches(hocon"""
             foo.bar : "10 minutes ago"
             y : "10 minutes ago"
           """) shouldBe false

      xMatchesY.matches(hocon"""
             foo.bar : "11 minutes ago"
             y : "10 minutes ago"
           """) shouldBe false
    }
  }
  "ComparePredicate.eq" should {
    "evaluate against another value within the document" in {
      val xMatchesY = ComparePredicate(JPath("x"), JPath("y"), Op.Equals)

      xMatchesY.matches(hocon"""
             x : 1
             y : 2
           """) shouldBe false

      xMatchesY.matches(hocon"""
             x : 2
             y : 2
           """) shouldBe true
    }
  }
  "ComparePredicate.lt" should {
    "evaluate against another value within the document" in {

      val xLTy = ComparePredicate(JPath("x"), JPath("y"), Op.LT)

      xLTy.matches(hocon"""
             x : 1
             y : 2
           """) shouldBe true

      xLTy.matches(hocon"""
             x : 2
             y : 2
           """) shouldBe false

      xLTy.matches(hocon"""
             x : 3
             y : 2
           """) shouldBe false
    }
  }
  "ComparePredicate.lte" should {
    "evaluate against another value within the document" in {

      val xLTEy = ComparePredicate(JPath("x"), JPath("y"), Op.LTE)
      xLTEy.matches(hocon"""
             x : 1
             y : 2
           """) shouldBe true

      xLTEy.matches(hocon"""
             x : 2
             y : 2
           """) shouldBe true

      xLTEy.matches(hocon"""
             x : 3
             y : 2
           """) shouldBe false
    }
  }
  "ComparePredicate.gt" should {
    "evaluate against another value within the document" in {

      val xGTy = ComparePredicate(JPath("x"), JPath("y"), Op.GT)
      xGTy.matches(hocon"""
             x : 1
             y : 2
           """) shouldBe false

      xGTy.matches(hocon"""
             x : 2
             y : 2
           """) shouldBe false

      xGTy.matches(hocon"""
             x : 3
             y : 2
           """) shouldBe true
    }
  }
  "ComparePredicate.gte" should {
    "evaluate against another value within the document" in {

      val xGTEy = ComparePredicate(JPath("x"), JPath("y"), Op.GTE)

      xGTEy.matches(hocon"""
             x : 1
             y : 2
           """) shouldBe false

      xGTEy.matches(hocon"""
             x : 2
             y : 2
           """) shouldBe true

      xGTEy.matches(hocon"""
             x : 3
             y : 2
           """) shouldBe true
    }
  }
}
