package donovan.json

import donovan.json.JType._
import io.circe.Json

object JsonForSchema {

  /** @param schema the schema
    * @param jsonForType a function which will create the json values for a [[JType]]
    * @return an example json document based on the input schema
    */
  def apply(schema: TypesByPath)(implicit jsonForType: JType => Json = defaultJsonForType): Json = {
    if (schema.isEmpty) {
      Json.Null
    } else {
      val jsons = schema.map {
        case (path, typ) => jsonForPath(path, typ)(jsonForType)
      }
      jsons.reduce(donovan.json.deepMergeWithArrayConcat)
    }
  }

  def defaultJsonForType(typ: JType): Json = {
    typ match {
      case NullType => Json.Null
      case BooleanType => Json.True
      case NumericType => Json.fromInt(123)
      case TextType => Json.fromString("text")
      case ArrayType => Json.arr()
      case ObjType => Json.obj()
    }
  }

  private val IsArray = "(.*)\\[\\]".r

  def jsonForPath(path: List[String], typ: JType)(jsonForType: JType => Json): Json = {
    path match {
      case Nil => jsonForType(typ)
      case IsArray(field) :: tail =>
        val nested = jsonForPath(tail, typ)(jsonForType)
        Json.obj(field -> Json.arr(nested))
      case field :: tail =>
        Json.obj(field -> jsonForPath(tail, typ)(jsonForType))
    }
  }


}
