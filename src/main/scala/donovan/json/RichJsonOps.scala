package donovan.json

import io.circe.Json

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

  /** @return a collection of json types by their 'jpaths'
    */
  def typesByPath: TypesByPath = schema.flattenPaths

  /** @return the TypeNode, whose '.toString' can be handy to print schemas
    */
  def schema: TypeNode = TypeNode(json)

  /** @return a new json document with the values replaced
    */
  def anonymize(implicit jsonForType: JType => Json = JType.defaultJsonForType): Json = schema.anonymize()

}

object RichJsonOps {

  trait LowPriorityJsonOpsImplicits {
    implicit def asRichJson(json: Json): RichJsonOps = RichJsonOps(json)
  }

  object implicits extends LowPriorityJsonOpsImplicits

}
