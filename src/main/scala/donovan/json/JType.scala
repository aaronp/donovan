package donovan.json
import io.circe.Decoder.Result
import io.circe._

/**
  * A class representing the available json types
  */
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

  implicit object JsonFormat extends Encoder[JType] with Decoder[JType] {
    private val _Null    = "Null"
    private val _Boolean = "Boolean"
    private val _Numeric = "Numeric"
    private val _Text    = "Text"
    private val _Array   = "Array"
    private val _Object  = "Object"

    override def apply(typ: JType): Json = {
      val name = typ match {
        case TextType    => _Text
        case ObjType     => _Object
        case NumericType => _Numeric
        case BooleanType => _Boolean
        case NullType    => _Null
        case ArrayType   => _Array
      }
      Json.fromString(name)
    }

    override def apply(c: HCursor): Result[JType] = {
      import cats.syntax.either._
      c.as[String].flatMap {
        case `_Text`    => TextType.asRight
        case `_Numeric` => NumericType.asRight
        case `_Object`  => ObjType.asRight
        case `_Boolean` => BooleanType.asRight
        case `_Null`    => NullType.asRight
        case `_Array`   => ArrayType.asRight
        case other      => DecodingFailure(s"Couldn't parse string as type: '$other'", c.history).asLeft
      }
    }
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
