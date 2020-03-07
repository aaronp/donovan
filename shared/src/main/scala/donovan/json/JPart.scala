package donovan.json

import io.circe.Decoder.Result
import io.circe._
import io.circe.syntax._

/**
  * Represents part of a jpath.
  *
  */
sealed trait JPart {

  final def asPath = JPath(this)

  final def +:(other: JPath) = JPath(this +: other.path)

  final def :+(other: JPath) = JPath(other.path :+ this)

  final def asMatcher(filter: JPredicate = JPredicate.matchAll): JPredicate = asPath.asMatcher(filter)

  final def and(other: JPredicate): JPredicate = asMatcher().and(other)

  final def and(other: JPart): JPredicate = and(other.asMatcher())

  final def or(other: JPredicate): JPredicate = asMatcher().or(other)

  final def or(other: JPart): JPredicate = or(other.asMatcher())

  final def asPos: Option[JPos] = this match {
    case p: JPos => Option(p)
    case _       => None
  }
  final def asField: Option[JField] = this match {
    case p: JField => Option(p)
    case _         => None
  }
  final def asFind: Option[JArrayFind] = this match {
    case p: JArrayFind => Option(p)
    case _             => None
  }
  final def asFilter: Option[JFilter] = this match {
    case p: JFilter => Option(p)
    case _          => None
  }
}

object JPart {

  def apply(name: String) = JField(name)

  def apply(i: Int) = JPos(i)

  def apply(field: String, predicate: JPredicate) = JFilter(field, predicate)

  import cats.syntax.either._

  implicit def intAsPos(pos: Int) = JPos(pos)

  object JFilterDec extends Decoder[JFilter] {
    override def apply(c: HCursor): Result[JFilter] = {
      val fldCurs = c.downField("field").as[String]
      val prdCurs = c.downField("predicate").as[JPredicate](JPredicate.JPredicateFormat)
      fldCurs.flatMap { (fld: String) =>
        prdCurs.map { (prd: JPredicate) =>
          JFilter(fld, prd)
        }
      }
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
          Json.obj("field" -> Json.fromString(filter.field), "predicate" -> filter.predicate.json)
        case JField(field)   => field.asJson
        case JPos(pos)       => pos.asJson
        case pos: JArrayFind => pos.asJson
      }
    }
  }

}

case class JField(name: String) extends JPart
object JField {
  implicit val encoder: ObjectEncoder[JField] = io.circe.generic.semiauto.deriveEncoder[JField]
  implicit val decoder: Decoder[JField]       = io.circe.generic.semiauto.deriveDecoder[JField]
}

case class JPos(pos: Int) extends JPart
object JPos {
  implicit val encoder: ObjectEncoder[JPos] = io.circe.generic.semiauto.deriveEncoder[JPos]
  implicit val decoder: Decoder[JPos]       = io.circe.generic.semiauto.deriveDecoder[JPos]
}

case class JArrayFind(arrayFind: JPredicate) extends JPart
object JArrayFind {
  implicit val encoder: ObjectEncoder[JArrayFind] = io.circe.generic.semiauto.deriveEncoder[JArrayFind]
  implicit val decoder: Decoder[JArrayFind]       = io.circe.generic.semiauto.deriveDecoder[JArrayFind]
}

case class JFilter(field: String, predicate: JPredicate) extends JPart
object JFilter {
  implicit val encoder: ObjectEncoder[JFilter] = io.circe.generic.semiauto.deriveEncoder[JFilter]
  implicit val decoder: Decoder[JFilter]       = io.circe.generic.semiauto.deriveDecoder[JFilter]
}
