package donovan.json

import donovan.json.JType._
import io.circe.{Json, JsonObject}

import scala.compat.Platform

/**
  * Represents the type for a particular path
  */
sealed trait TypeNode {

  override def toString = flatten.sorted.mkString(Platform.EOL)

  private final def render(path: TypeByPath): String = {
    def show(p: String) = p match {
      case "" => "[]"
      case nonEmpty => nonEmpty
    }

    def showType(t: JType) = t match {
      case ArrayType => "[]"
      case nonEmpty => nonEmpty.toString
    }

    path match {
      case (Nil, t) => showType(t)
      case (head :: Nil, ArrayType) => s"${show(head)}.[]"
      case (head :: Nil, t) => s"${show(head)}:${showType(t)}"
      case (head :: tail, t) => s"${show(head)}.${render(tail, t)}"
    }
  }

  final def flatten: Vector[String] = flattenPaths.map(render)

  def flattenPaths: TypesByPath
}

object TypeNode {

  val Empty = TypeNodeValue(NullType)

  def apply(jType: JType) = TypeNodeValue(jType)

  def apply(json: Json): TypeNode = forJson(json)

  private def forObject(json: JsonObject): TypeNode = {
    TypeNodeObject(json.toMap.mapValues(forJson))
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
}

case class TypeNodeArray(children: Vector[TypeNode]) extends TypeNode {
  val `type`: JType = ArrayType

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

case class TypeNodeValue(val `type`: JType) extends TypeNode {
  override def flattenPaths: TypesByPath = Vector(Nil -> `type`)
}
