package donovan.json

import donovan.core.IsEmpty

object JsonDiffIsEmpty extends IsEmpty[JsonDiff] {
  override def isEmpty(diff: JsonDiff): Boolean = {
    diff.deltas.isEmpty
  }
}
