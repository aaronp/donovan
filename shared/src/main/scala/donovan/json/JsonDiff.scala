package donovan.json

import cats.Semigroup
import io.circe.{Decoder, Json, ObjectEncoder}

case class JsonDiff(deltas: List[DiffEntry]) {

  def strip(lhs: Json): Json = {
    import donovan.implicits._
    lhs.filter(deltas.map(_.jPath))
  }

  def size = deltas.size

  def isEmpty = deltas.isEmpty

  def asDelta: JsonDelta = {
    val removeAllOldValues = deltas.map {
      case DiffEntry(path, _, _) => JPath.forParts(path)
    }
    val entriesToAdd: List[Json] = deltas.collect {
      case DiffEntry(path, _, rhs) if !rhs.isNull =>
        path.foldRight(rhs) {
          case (p, json) => Json.obj(p -> json)
        }
    }
    val add = entriesToAdd match {
      case Nil => Json.Null
      case head :: tail =>
        tail.foldLeft(head) {
          case (a, b) => deepMergeWithArrayConcat(a, b)
        }
    }
    JsonDelta(removeAllOldValues, add)
  }
}

object JsonDiff {

  implicit val encoder: ObjectEncoder[JsonDiff] = io.circe.generic.semiauto.deriveEncoder[JsonDiff]
  implicit val decoder: Decoder[JsonDiff]       = io.circe.generic.semiauto.deriveDecoder[JsonDiff]

  def apply(lhs: Json, rhs: Json): JsonDiff = {
    val diffs = diffRecursive(Nil, lhs, rhs, Nil)
    new JsonDiff(diffs)
  }

  def apply(only: Json): JsonDiff = {
    if (only.isNull) {
      JsonDiff(Nil)
    } else {
      JsonDiff(List(DiffEntry(Nil, Json.Null, only)))
    }
  }

  private def arrayDiff(path: List[String], lhsArray: Vector[Json], rhsArray: Vector[Json], diffs: List[DiffEntry]) = {
    // TODO - cleverer array diff, like longest prefix
    if (lhsArray != rhsArray) {
      DiffEntry(path, Json.fromValues(lhsArray), Json.fromValues(rhsArray)) :: diffs
    } else {
      diffs
    }
  }

  private def diffRecursive(path: List[String], lhsIn: Json, rhsIn: Json, diffs: List[DiffEntry]): List[DiffEntry] = {
    (lhsIn.asObject, rhsIn.asObject) match {
      case (Some(lhsObj), Some(rhsObj)) =>
        val leftMap  = lhsObj.toMap
        val rightMap = rhsObj.toMap
        val keys     = leftMap.keySet ++ rightMap.keySet
        keys.foldLeft(diffs) {
          case (diffList, key) =>
            (leftMap.get(key), rightMap.get(key)) match {
              case (Some(a), Some(b)) => diffRecursive(path :+ key, a, b, diffList)
              case (Some(a), None)    => DiffEntry(path :+ key, a, Json.Null) :: diffList
              case (None, Some(b))    => DiffEntry(path :+ key, Json.Null, b) :: diffList
              case (None, None)       => sys.error(s"map is broken for $key in $leftMap and $rightMap")
            }
        }
      case _ =>
        (lhsIn.asArray, rhsIn.asArray) match {
          case (Some(lhsArray), Some(rhsArray)) => arrayDiff(path, lhsArray, rhsArray, diffs)
          case _ =>
            if (lhsIn == rhsIn) {
              diffs
            } else {
              DiffEntry(path, lhsIn, rhsIn) :: diffs
            }
        }
    }
  }

  implicit object JsonDiffSemigroup extends Semigroup[JsonDiff] {
    override def combine(x: JsonDiff, y: JsonDiff): JsonDiff = {
      JsonDiff((x.deltas ++ y.deltas).distinct)
    }
  }

}

case class DiffEntry(path: List[String], lhs: Json, rhs: Json) {
  def jPath = JPath.forParts(path)
}

object DiffEntry {
  def apply(lhs: Json, rhs: Json): DiffEntry = new DiffEntry(Nil, lhs, rhs)

  implicit val encoder: ObjectEncoder[DiffEntry] = io.circe.generic.semiauto.deriveEncoder[DiffEntry]
  implicit val decoder: Decoder[DiffEntry]       = io.circe.generic.semiauto.deriveDecoder[DiffEntry]

}
