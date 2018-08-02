package donovan.json

import io.circe.Json

import scala.runtime.RichDouble

case class RichJsonOps(json: Json) extends AnyVal {

  /**
    * @param path    the first jpath to filter on
    * @param theRest the rest of the paths to filter
    * @return json which only contains values which match the supplied paths
    */
  def filter(path: JPath, theRest: JPath*): Json = filter(path :: theRest.toList)

  /**
    * @param paths paths to include
    * @return json which only contains values which match the supplied paths
    */
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
