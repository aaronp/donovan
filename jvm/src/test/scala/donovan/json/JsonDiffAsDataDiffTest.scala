package donovan.json
import donovan.BaseJsonSpec
import io.circe.Json

class JsonDiffAsDataDiffTest extends BaseJsonSpec {

  "JsonDiffAsDataDiff" ignore {
    "provide the difference between two types as a delta" in {

      val lhs = hocon"""{
          flag : true
          name : foo
          age : 123
          deep {
            array : [1,2,3],
            nested : [
              { id : 1 },
              { id : 2 }
            ]
          }
              }"""

      val rhs = hocon"""{
          flag : false
          name : foo
          age : 456
          deep {
            array : [1,3],
            nested : [
              { id : 1 },
              { id : 3 }
            ]
          }
              }"""

      JsonDiffAsDataDiff.diff(lhs, lhs).deltas shouldBe (empty)
      JsonDiffAsDataDiff.diff(rhs, rhs).deltas shouldBe (empty)

      val deltas: Seq[DiffEntry] = JsonDiffAsDataDiff.diff(lhs, rhs).deltas

      deltas should contain(
        DiffEntry(List("flag"), Json.True, Json.False),
        DiffEntry(List("age"), Json.fromInt(123), Json.fromInt(456)),
        DiffEntry(List("deep", "array"), Json.arr(Json.fromInt(1), Json.fromInt(2)), Json.arr(Json.fromInt(1), Json.fromInt(3))),
      )
    }
  }
}
