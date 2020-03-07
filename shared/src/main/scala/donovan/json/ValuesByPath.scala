package donovan.json

import io.circe.{Encoder, HCursor, Json, JsonObject}

object ValuesByPath {
  def apply[A: Encoder](value: A): Map[ValuePath, Json] = {
    import io.circe.syntax._
    forJson(value.asJson)
  }

  def forJson(json: Json): Map[ValuePath, Json] = {
    select(json.hcursor, Nil, Map.empty).map {
      case (path, value) => path -> value
    }
  }

  private def select(cursor: HCursor, currentPath: ValuePath, valuesByPath: Map[ValuePath, Json]): Map[ValuePath, Json] = {

    cursor.focus match {
      case None => valuesByPath
      case Some(currentJson) =>
        def returnMap: Map[ValuePath, Json] = valuesByPath.updated(currentPath, currentJson)

        def fromObj(obj: JsonObject): Map[ValuePath, Json] = {
          obj.toMap.foldLeft(valuesByPath) {
            case (map, (name, _)) =>
              val arrayPath = currentPath :+ name
              cursor.downField(name) match {
                case newCursor: HCursor => select(newCursor, arrayPath, map)
                case _ => cursor.focus.fold(map)(map.updated(arrayPath, _))
              }
          }
        }

        def zip(array: Vector[Json]): Map[ValuePath, Json] = {
          array.zipWithIndex.foldLeft(valuesByPath) {
            case (map, (_, i)) =>
              val arrayPath = currentPath :+ s"[$i]"
              cursor.downN(i) match {
                case h: HCursor => select(h, arrayPath, map)
                case _ => map
              }
          }
        }

        currentJson.arrayOrObject(returnMap,
          zip,
          fromObj
        )
    }
  }
}
