package donovan.json

import io.circe.Json

import scala.runtime.RichDouble

case class RichJsonOps(json: Json) extends AnyVal {

  def filter(path: JPath, theRest: JPath*): Json = filter(path :: theRest.toList)

  def filter(paths: Iterable[JPath]): Json = {
    val parts: Iterable[Json] = paths.flatMap(_.selectJson(json))
    if (parts.isEmpty) {
      Json.Null
    } else {
      parts.reduce(deepMergeWithArrayConcat)
    }
  }

  def paths: Vector[JPath] = typesByPath.map {
    case (path, _) => JPath.forParts(path)
  }

  def typesByPath: TypesByPath = TypeNode(json).flattenPaths

}

object RichJsonOps {

  trait LowPriorityJsonOpsImplicits {
    implicit def asRichJson(json: Json): RichJsonOps = RichJsonOps(json)
  }

  object implicits extends LowPriorityJsonOpsImplicits

}
