package donovan.json

import donovan.time.{DateTimeResolver, TimeCoords, Timestamp}
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Json, _}

import scala.language.implicitConversions
import scala.util.Try

/**
  * Represents something which can match some Json
  */
sealed trait JPredicate { self =>

  /** @param json the json to match
    * @return true if this predicate matches the given json
    */
  def matches(json: Json): Boolean

  /** @return a json representation of this predicate
    */
  def json: Json

  def and(other: JPredicate, theRest: JPredicate*): JPredicate =
    new And(self :: other :: theRest.toList)

  def or(other: JPredicate, theRest: JPredicate*): JPredicate =
    new Or(self :: other :: theRest.toList)

  def unary_! : JPredicate = Not(this)

  final def inArray: JArrayFind = JArrayFind(this)
}

object JPredicate {

  def matchAll: JPredicate = MatchAll

  def matchNone: JPredicate = MatchNone

  def apply(select: JPath, test: JPredicate = JPredicate.matchAll) = TestPredicate(select, test)

  implicit def pathAsMatcher(jpath: JPath): JPredicate = jpath.asMatcher()

  object implicits extends LowPriorityPredicateImplicits

  trait LowPriorityPredicateImplicits {

    implicit def boolAsJson(b: Boolean) = Json.fromBoolean(b)

    implicit def stringAsJson(s: String) = Json.fromString(s)

    implicit def intAsJson(i: Int) = Json.fromInt(i)

    implicit def jsonInArray[T](value: T)(implicit ev: T => Json) = new {

      def inArray: JArrayFind = Eq(ev(value)).inArray
    }

    implicit class RichJsonField(val field: String) {
      def asJPath: JPath = JPath(field)
      implicit def asJField = JField(field)
    }

  }

  implicit object JPredicateFormat extends Encoder[JPredicate] with Decoder[JPredicate] {

    private def asMatchAllOrNone(c: HCursor): Result[JPredicate] = c.as[String] match {
      case Right("match-all")  => Right(MatchAll): Result[JPredicate]
      case Right("match-none") => Right(MatchNone): Result[JPredicate]
      case Right(other) =>
        Left(DecodingFailure(s"Expected 'match-all' or 'match-none', but got '$other'", c.history)): Result[JPredicate]
      case left: Result[_] => left.asInstanceOf[Result[JPredicate]]
    }

    override def apply(c: HCursor): Result[JPredicate] = {
      import cats.syntax.either._

      // format: off
      c.as[TestPredicate].
        orElse(asMatchAllOrNone(c)).
        orElse(c.as[ComparePredicate]).
        orElse(c.as[And]).
        orElse(c.as[Or]).
        orElse(c.as[Not]).
        orElse(c.as[Eq]).
        orElse(c.as[JRegex]).
        orElse(c.as[JIncludes]).
        orElse(c.as[Gt]).
        orElse(c.as[Gte]).
        orElse(c.as[Lt]).
        orElse(c.as[Lte]).
        orElse(c.as[Before]).
        orElse(c.as[After])
      // format: on
    }

    override def apply(a: JPredicate): Json = a match {
      case p: TestPredicate    => p.asJson
      case p: ComparePredicate => p.asJson
      case MatchAll            => MatchAll.json
      case MatchNone           => MatchNone.json
      case p: And              => p.asJson
      case p: Or               => p.asJson
      case p: Not              => p.asJson
      case p: Eq               => p.asJson
      case p: JRegex           => p.asJson
      case p: JIncludes        => p.asJson

      case p: Gt  => p.asJson
      case p: Gte => p.asJson
      case p: Lt  => p.asJson
      case p: Lte => p.asJson

      case p: Before => p.asJson
      case p: After  => p.asJson
    }
  }

}

case object MatchAll extends JPredicate {
  override def matches(json: Json): Boolean = true

  override def json = Json.fromString("match-all")

  override def and(other: JPredicate, theRest: JPredicate*): JPredicate = {
    theRest match {
      case Seq()        => other
      case head +: tail => other.and(head, tail: _*)
    }
  }

  override def or(other: JPredicate, theRest: JPredicate*): JPredicate = this
}

case object MatchNone extends JPredicate {
  override def matches(json: Json): Boolean = false

  override def json = Json.fromString("match-none")

  override def and(other: JPredicate, theRest: JPredicate*): JPredicate = this

  override def or(other: JPredicate, theRest: JPredicate*): JPredicate = {
    theRest match {
      case Seq()        => other
      case head +: tail => other.or(head, tail: _*)
    }
  }
}

case class Or(or: List[JPredicate]) extends JPredicate {
  override def matches(json: Json) = or.exists(_.matches(json))

  override def json = Json.obj("or" -> Json.fromValues(or.map(_.json)))
}

object Or {
  def apply(first: JPredicate, second: JPredicate, theRest: JPredicate*): Or =
    Or(first :: second :: theRest.toList)
}

case class And(and: List[JPredicate]) extends JPredicate {
  override def matches(json: Json) = and.forall(_.matches(json))

  override def json = Json.obj("and" -> Json.fromValues(and.map(_.json)))
}

object And {
  def apply(first: JPredicate, second: JPredicate, theRest: JPredicate*): And =
    And(first :: second :: theRest.toList)
}

case class Not(not: JPredicate) extends JPredicate {
  override def matches(json: Json) = !(not.matches(json))

  override def unary_! = not

  override def json: Json = this.asJson
}

case class Eq(eq: Json) extends JPredicate {
  override def matches(json: Json) = json == eq

  override def json: Json = this.asJson
}

case class Before(before: String) extends TimePredicate(before, _ isBefore _) with JPredicate {
  override def json: Json = this.asJson
}

case class After(after: String) extends TimePredicate(after, _ isAfter _) with JPredicate {
  override def json: Json = this.asJson
}

abstract class TimePredicate(time: String, compare: (Timestamp, Timestamp) => Boolean) {
  private val adjust: DateTimeResolver = time match {
    case TimeCoords(f) => f
    case other         => sys.error(s"'$time' couldn't be parsed as a date-time adjustment: $other")
  }

  def matches(json: Json) = {
    json.asString.exists {
      case TimeCoords(valueAdjust) =>
        val now: Timestamp = TimeCoords.nowUTC()
        val jsonTime       = valueAdjust(now)
        val reference      = adjust(now)
        compare(jsonTime, reference)
      case _ => false
    }
  }
}

case class JRegex(regex: String) extends JPredicate {
  private val pattern = regex.r

  override def matches(json: Json) = json.asString.exists(v => pattern.findFirstIn(v).isDefined)

  override def json: Json = this.asJson
}

case class JIncludes(elements: Set[Json]) extends JPredicate {

  def contains(array: Vector[Json]): Boolean = elements.forall(array.contains)

  override def matches(json: Json) = json.asArray.exists(contains)

  override def json: Json = this.asJson
}

sealed abstract class ComparablePredicate(value: Json, bdCompare: (BigDecimal, BigDecimal) => Boolean, longCompare: (Long, Long) => Boolean)
    extends JPredicate {
  //  TODO - we could compare the (private) Json instance types instead of using this 'toString' hack
  val requiresDec: Boolean                   = value.asNumber.map(_.toString).exists(_.contains("."))
  lazy val refLong: Option[Long]             = asLong(value)
  lazy val refBigDecimal: Option[BigDecimal] = asBigDecimal(value)

  private def asLong(json: Json) = {
    json.asNumber.flatMap(_.toLong).orElse {
      json.asString.flatMap(s => Try(s.toLong).toOption)
    }
  }

  private def asBigDecimal(json: Json): Option[BigDecimal] = {
    json.asNumber.flatMap(_.toBigDecimal).orElse {
      json.asString.flatMap(s => Try(BigDecimal(s)).toOption)
    }
  }

  override def matches(json: Json): Boolean = {
    if (requiresDec) {
      (asBigDecimal(json), refBigDecimal) match {
        case (Some(x), Some(y)) => bdCompare(x, y)
        case _                  => false
      }
    } else {
      (asLong(json), refLong) match {
        case (Some(x), Some(y)) => longCompare(x, y)
        case _                  => false
      }
    }
  }
}

import io.circe.Json

case class Gt(gt: Json) extends ComparablePredicate(gt, _ > _, _ > _) {
  override def json: Json = this.asJson
}

case class Gte(gte: Json) extends ComparablePredicate(gte, _ >= _, _ >= _) {
  override def json: Json = this.asJson
}

case class Lt(lt: Json) extends ComparablePredicate(lt, _ < _, _ < _) {
  override def json: Json = this.asJson
}

case class Lte(lte: Json) extends ComparablePredicate(lte, _ <= _, _ <= _) {
  override def json: Json = this.asJson
}

case class ComparePredicate(lhs: JPath, rhs: JPath, op: Op) extends JPredicate {
  override def matches(json: Json): Boolean = {
    lhs.selectValue(json).exists { left =>
      rhs.selectValue(json).exists { right =>
        op.eval(left, right)
      }
    }
  }

  override def toString = s"Test($lhs $op $rhs)"

  override def json = {
    Json.obj("lhs" -> lhs.json, "rhs" -> rhs.json, "op" -> Json.fromString(op.name))
  }
}

case class TestPredicate(select: JPath, test: JPredicate) extends JPredicate {
  override def matches(json: Json): Boolean = {
    select(json).exists(test.matches)
  }

  override def toString = s"Test($select, $test)"

  override def json = {
    Json.obj("select" -> select.json, "test" -> test.json)
  }
}
