package donovan.json
import io.circe.Json

sealed trait JType

object JType {
  case object NullType extends JType

  case object BooleanType extends JType

  case object NumericType extends JType

  case object TextType extends JType

  case object ArrayType extends JType

  case object ObjType extends JType

  def apply(json: Json): JType = {
    json.fold(
      NullType,
      _ => BooleanType,
      _ => NumericType,
      _ => TextType,
      _ => ArrayType,
      _ => ObjType
    )
  }

  def defaultJsonForType(typ: JType): Json = {
    typ match {
      case NullType    => Json.Null
      case BooleanType => Json.True
      case NumericType => Json.fromInt(123)
      case TextType    => Json.fromString("text")
      case ArrayType   => Json.arr()
      case ObjType     => Json.obj()
    }
  }
}
