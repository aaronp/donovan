package donovan.json

import cats.kernel.Semigroup
import io.circe.Json

object JsonSemigroup extends Semigroup[Json] {
  override def combine(x: Json, y: Json): Json = deepMergeWithArrayConcat(x, y)
}
