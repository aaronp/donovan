package donovan.json

import donovan.json.JType._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.extras.auto._
import io.circe.syntax._

import scala.compat.Platform

/**
  * Represents the type for a particular path
  */
sealed trait TypeNode {

  def anonymize(jsonForType: JType => Json = defaultJsonForType): Json

  def pretty: String = flatten.sorted.mkString(Platform.EOL)

  private final def render(path: TypeByPath): String = {
    def show(p: String) = p match {
      case ""       => "[]"
      case nonEmpty => nonEmpty
    }

    def showType(t: JType) = t match {
      case ArrayType => "[]"
      case nonEmpty  => nonEmpty.toString
    }

    path match {
      case (Nil, t)                 => showType(t)
      case (head :: Nil, ArrayType) => s"${show(head)}.[]"
      case (head :: Nil, t)         => s"${show(head)}:${showType(t)}"
      case (head :: tail, t)        => s"${show(head)}.${render(tail, t)}"
    }
  }

  final def flatten: Vector[String] = flattenPaths.map(render)

  def flattenPaths: TypesByPath
}

object TypeNode {

  val Empty = TypeNodeValue(NullType)

  def apply(jType: JType) = TypeNodeValue(jType)

  def apply(json: Json): TypeNode = forJson(json)

  implicit object Format extends Encoder[TypeNode] with Decoder[TypeNode] {
    override def apply(a: TypeNode): Json = {
      a match {
        case TypeNodeArray(members) => Json.arr(members.map(_.asJson): _*)
        case TypeNodeObject(children) =>
          val kids = children.map {
            case (k, v) => (k, v.asJson)
          }
          Json.obj(kids.toSeq: _*)
        case TypeNodeValue(typ) => typ.asJson
      }
    }

    override def apply(c: HCursor): Result[TypeNode] = {
      val either = c.as[JType].map(TypeNodeValue.apply) match {
        case ok @ Right(_) => ok
        case _ =>
          c.as[Array[TypeNode]].map { arr =>
            TypeNodeArray(arr.toVector)
          }
      }
      either match {
        case ok @ Right(_) => ok
        case _ =>
          c.as[Map[String, TypeNode]].map { obj =>
            TypeNodeObject(obj)
          }
      }
    }
  }

  private def forObject(json: JsonObject): TypeNode = {
    TypeNodeObject(json.toMap.mapValues(forJson).toMap)
  }

  private def forArray(json: Vector[Json]): TypeNode = {
    val arrayValues: Vector[TypeNode] = json.map(forJson).distinct
    TypeNodeArray(arrayValues)
  }

  private def forJson(json: Json): TypeNode = {
    json.arrayOrObject(apply(JType(json)), forArray, forObject)
  }
}

case class TypeNodeObject(children: Map[String, TypeNode]) extends TypeNode {
  val `type`: JType = ObjType

  override def flattenPaths: TypesByPath = {
    children.toVector.flatMap {
      case (key, array: TypeNodeArray) =>
        array.flattenPaths.map {
          case (path, t) => (key :: path) -> t
        }
      case (key, values) =>
        values.flattenPaths.map {
          case (path, t) => (key :: path) -> t
        }
    }
  }

  override def anonymize(jsonForType: JType => Json): Json = {
    val kids = children.mapValues(_.anonymize(jsonForType))
    Json.obj(kids.toSeq: _*)
  }
}

case class TypeNodeArray(children: Vector[TypeNode]) extends TypeNode {
  val `type`: JType = ArrayType

  override def anonymize(jsonForType: JType => Json): Json = {
    val kids = children.map(_.anonymize(jsonForType))
    Json.arr(kids: _*)
  }

  override def flattenPaths: TypesByPath = {
    if (children.isEmpty) {
      Vector(Nil -> ArrayType)
    } else {
      val nested: Vector[(List[String], JType)] = children.flatMap(_.flattenPaths)
      nested.map {
        case (path, t) => ("" +: path, t)
      }
    }
  }
}

case class TypeNodeValue(`type`: JType) extends TypeNode {
  override def anonymize(jsonForType: JType => Json): Json = jsonForType(`type`)
  override def flattenPaths: TypesByPath                   = Vector(Nil -> `type`)
}
