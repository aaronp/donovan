package donovan.json

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

/**
  * Represents part of a 'jpath' (e.g. a field, position in an array, or field which matches a given condition)
  *
  */
sealed trait JPart {

  final def asPath = JPath(this)

  final def +:(other : JPath) = JPath(this +: other.path)

  final def :+(other : JPath) = JPath(other.path :+ this)

  final def asMatcher(filter: JPredicate = JPredicate.matchAll): JPredicate = asPath.asMatcher(filter)

  final def and(other: JPredicate): JPredicate = asMatcher().and(other)

  final def and(other: JPart): JPredicate = and(other.asMatcher())

  final def or(other: JPredicate): JPredicate = asMatcher().or(other)

  final def or(other: JPart): JPredicate = or(other.asMatcher())

  final def asPos: Option[JPos] = this match {
    case p : JPos => Option(p)
    case _ => None
  }
  final def asField: Option[JField] = this match {
    case p : JField => Option(p)
    case _ => None
  }
  final def asFind: Option[JArrayFind] = this match {
    case p : JArrayFind => Option(p)
    case _ => None
  }
  final def asFilter: Option[JFilter] = this match {
    case p : JFilter => Option(p)
    case _ => None
  }
}

object JPart {

  def apply(name: String) = JField(name)

  def apply(i: Int) = JPos(i)

  def apply(predicate: JPredicate) = JFilter(predicate)

  import cats.syntax.either._

  implicit def intAsPos(pos: Int) = JPos(pos)

  object JFilterDec extends Decoder[JFilter] {
    override def apply(c: HCursor): Result[JFilter] = {
//      val prdCurs = c.downField("condition").as[JPredicate](JPredicate.JPredicateFormat)
      val prdCurs = c.as[JPredicate](JPredicate.JPredicateFormat)
      prdCurs.map(JFilter.apply)
    }
  }

  implicit object JPartFormat extends Decoder[JPart] with Encoder[JPart] {
    override def apply(c: HCursor): Result[JPart] = {
      val jposDec          = implicitly[Decoder[Int]].map(JPos.apply)
      val arrayContainsDec = implicitly[Decoder[JArrayFind]]
      val jfieldDec        = implicitly[Decoder[String]].map(JField.apply)
      jfieldDec
        .tryDecode(c)
        .orElse(jposDec.tryDecode(c))
        .orElse(arrayContainsDec.tryDecode(c))
        .orElse(JFilterDec.tryDecode(c))

    }

    override def apply(part: JPart): Json = {
      part match {
        case filter: JFilter =>
          //Json.obj("condition" -> filter.predicate.json)
          filter.predicate.json
        case JField(field)   => field.asJson
        case JPos(pos)       => pos.asJson
        case pos: JArrayFind => pos.asJson
      }
    }
  }

}

case class JField(name: String) extends JPart

case class JPos(pos: Int) extends JPart

case class JArrayFind(arrayFind: JPredicate) extends JPart

case class JFilter(predicate: JPredicate) extends JPart
