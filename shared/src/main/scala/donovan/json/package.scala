package donovan

import _root_.io.circe.Decoder.Result
import _root_.io.circe.Json.fromJsonObject
import _root_.io.circe._
import _root_.io.circe.syntax._
import cats.kernel.Semigroup
import donovan.core.FieldSelector

package object json {


  type ValuePath = Seq[String]
  type TypeByPath  = (List[String], JType)
  type TypesByPath = Vector[TypeByPath]
  def newTypesByPath() = Vector[TypeByPath]()

  implicit object TypesByPathSemigroup extends Semigroup[TypesByPath] {
    override def combine(x: TypesByPath, y: TypesByPath): TypesByPath = {
      (x ++ y).distinct
    }
  }
  implicit class RichDec[T](val result: Result[T]) extends AnyVal {
    def orDecode[A <: T: Decoder](c: HCursor): Result[T] = {
      val aDec: Decoder[A] = implicitly[Decoder[A]]
      result.left.flatMap { _ =>
        aDec.tryDecode(c)
      }
    }
  }

  def jsonSelectorForPath(path: JPath): FieldSelector[Json, Json] = {
    FieldSelector.lift[Json, Json] { data =>
      path.apply(data).getOrElse(Json.Null)
    }
  }

  val JsonDiffAsDeltas = JsonDiffAsDataDiff.map { jsonDiff: JsonDiff =>
    jsonDiff.asJson
  }

  def deepMergeWithArrayConcat(targetJson: Json, jsonToAdd: Json): Json = {

    def notObjectMerge = {
      (targetJson.asArray, jsonToAdd.asArray) match {
        case (Some(leftArray), Some(rightArray)) => Json.fromValues(leftArray ++ rightArray)
        case (Some(leftArray), None)             => Json.fromValues(leftArray :+ jsonToAdd)
        case (None, Some(rightArray))            => Json.fromValues(targetJson +: rightArray)
        case _                                   => jsonToAdd
      }
    }

    (targetJson.asObject, jsonToAdd.asObject) match {
      case (Some(lhs), Some(rhs)) =>
        fromJsonObject(
          lhs.toList.foldLeft(rhs) {
            case (acc, (key, value)) =>
              val concatenatedOpt: Option[JsonObject] = for {
                leftArray  <- value.asArray
                rightArray <- rhs(key).flatMap(_.asArray)
              } yield {
                val arr = Json.fromValues(leftArray ++ rightArray)
                acc.add(key, arr)
              }

              def fallback = rhs(key).fold(acc.add(key, value)) { r =>
                acc.add(key, deepMergeWithArrayConcat(value, r))
              }

              concatenatedOpt.getOrElse(fallback)
          }
        )
      case _ => notObjectMerge
    }
  }

}
