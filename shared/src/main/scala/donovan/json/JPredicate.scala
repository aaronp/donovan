package donovan.json

import donovan.time.{DateTimeResolver, TimeCoords, Timestamp}
import io.circe.Decoder.Result
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

  final def inArray = JArrayFind(this)
}

object JPredicate {

  def matchAll: JPredicate = MatchAll

  def matchNone: JPredicate = MatchNone

  def apply(select: JPath, test: JPredicate = JPredicate.matchAll) = TestPredicate(select, test)

  implicit def filterAsMatcher(filter: JFilter): JPredicate = filter.asMatcher()

  implicit def pathAsMatcher(jpath: JPath): JPredicate = jpath.asMatcher()

  object implicits extends LowPriorityPredicateImplicits

  trait LowPriorityPredicateImplicits {

    implicit def boolAsJson(b: Boolean) = Json.fromBoolean(b)

    implicit def stringAsJson(s: String) = Json.fromString(s)

    implicit def intAsJson(i: Int) = Json.fromInt(i)

    implicit def jsonInArray[T](value: T)(implicit ev: T => Json) = new {

      def inArray: JArrayFind = Eq(ev(value)).inArray
    }

    implicit class RichJsonField(field: String) {
      private implicit def predAsJFilter(p: JPredicate): JPath = {
        val path = field.asJPath.path
        val opt = path.last.asField.map(_.name).map { lastFieldName =>
          val prefix: List[JPart] = path.init

          val filter = JFilter(lastFieldName, p)
          JPath(prefix :+ filter)
        }
        opt.getOrElse(JFilter(field, p).asPath)
      }

      def asJPath = JPath(field)

      def asJField = JField(field)

      def !(other: JPredicate): JPath = Not(other)

      def =!=[J](value: J)(implicit ev: J => Json): JPath = Not(Eq(value))

      def !==[J](value: J)(implicit ev: J => Json): JPath = {
        =!=(value)
      }

      def ===[J](value: J)(implicit ev: J => Json): JPath = Eq(value)

      def equalTo[J](value: J)(implicit ev: J => Json): JPath = Eq(value)

      def before(time: String): JPath = Before(time)

      def after(time: String): JPath = After(time)

      def gt[J](value: J)(implicit ev: J => Json): JPath = Gt(value)

      def lt[J](value: J)(implicit ev: J => Json): JPath = Lt(value)

      def gte[J](value: J)(implicit ev: J => Json): JPath = Gte(value)

      def lte[J](value: J)(implicit ev: J => Json): JPath = Lte(value)

      def ~=(regex: String): JPath = JRegex(regex)

      def includes[J](items: Set[J])(implicit ev: J => Json): JPath = JIncludes(items.map(ev))

      def includes[J](first: J, theRest: J*)(implicit ev: J => Json): JPath =
        includes(theRest.toSet + first)
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
      case p: TestPredicate => p.asJson
      case MatchAll         => MatchAll.json
      case MatchNone        => MatchNone.json
      case p: And           => p.asJson
      case p: Or            => p.asJson
      case p: Not           => p.asJson
      case p: Eq            => p.asJson
      case p: JRegex        => p.asJson
      case p: JIncludes     => p.asJson

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
  implicit val encoder: ObjectEncoder[Or] = io.circe.generic.semiauto.deriveEncoder[Or]
  implicit val decoder: Decoder[Or]       = io.circe.generic.semiauto.deriveDecoder[Or]
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

  implicit val encoder: ObjectEncoder[And] = io.circe.generic.semiauto.deriveEncoder[And]
  implicit val decoder: Decoder[And]       = io.circe.generic.semiauto.deriveDecoder[And]
}

case class Not(not: JPredicate) extends JPredicate {
  override def matches(json: Json) = !(not.matches(json))

  override def unary_! = not

  override def json: Json = this.asJson
}
object Not {

  implicit val encoder: ObjectEncoder[Not] = io.circe.generic.semiauto.deriveEncoder[Not]
  implicit val decoder: Decoder[Not]       = io.circe.generic.semiauto.deriveDecoder[Not]
}

case class Eq(eq: Json) extends JPredicate {
  override def matches(json: Json) = json == eq

  override def json: Json = this.asJson
}
object Eq {

  implicit val encoder: ObjectEncoder[Eq] = io.circe.generic.semiauto.deriveEncoder[Eq]
  implicit val decoder: Decoder[Eq]       = io.circe.generic.semiauto.deriveDecoder[Eq]
}

case class Before(before: String) extends TimePredicate(before, _ isBefore _) with JPredicate {
  override def json: Json = this.asJson
}
object Before {

  implicit val encoder: ObjectEncoder[Before] = io.circe.generic.semiauto.deriveEncoder[Before]
  implicit val decoder: Decoder[Before]       = io.circe.generic.semiauto.deriveDecoder[Before]
}

case class After(after: String) extends TimePredicate(after, _ isAfter _) with JPredicate {
  override def json: Json = this.asJson
}
object After {

  implicit val encoder: ObjectEncoder[After] = io.circe.generic.semiauto.deriveEncoder[After]
  implicit val decoder: Decoder[After]       = io.circe.generic.semiauto.deriveDecoder[After]
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
object JRegex {

  implicit val encoder: ObjectEncoder[JRegex] = io.circe.generic.semiauto.deriveEncoder[JRegex]
  implicit val decoder: Decoder[JRegex]       = io.circe.generic.semiauto.deriveDecoder[JRegex]
}

case class JIncludes(elements: Set[Json]) extends JPredicate {

  def contains(array: Vector[Json]): Boolean = elements.forall(array.contains)

  override def matches(json: Json) = json.asArray.exists(contains)

  override def json: Json = this.asJson
}
object JIncludes {

  implicit val encoder: ObjectEncoder[JIncludes] = io.circe.generic.semiauto.deriveEncoder[JIncludes]
  implicit val decoder: Decoder[JIncludes]       = io.circe.generic.semiauto.deriveDecoder[JIncludes]
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

  private def asBigDecimal(json: Json) = {
    json.asNumber.flatMap(_.toBigDecimal).orElse {
      json.asString.flatMap(s => Try(BigDecimal(s)).toOption)
    }
  }

  override def matches(json: Json): Boolean = {
    val res = json.as[Json].right.map { (tea: Json) =>
      if (requiresDec) {
        (asBigDecimal(tea), refBigDecimal) match {
          case (Some(x), Some(y)) => bdCompare(x, y)
          case _                  => false
        }
      } else {
        (asLong(tea), refLong) match {
          case (Some(x), Some(y)) => longCompare(x, y)
          case _                  => false
        }
      }
    }
    res.right.getOrElse(false)
  }
}

import io.circe.Json

case class Gt(gt: Json) extends ComparablePredicate(gt, _ > _, _ > _) {
  override def json: Json = this.asJson
}
object Gt {
  implicit val encoder: ObjectEncoder[Gt] = io.circe.generic.semiauto.deriveEncoder[Gt]
  implicit val decoder: Decoder[Gt]       = io.circe.generic.semiauto.deriveDecoder[Gt]
}

case class Gte(gte: Json) extends ComparablePredicate(gte, _ >= _, _ >= _) {
  override def json: Json = this.asJson
}
object Gte {
  implicit val encoder: ObjectEncoder[Gte] = io.circe.generic.semiauto.deriveEncoder[Gte]
  implicit val decoder: Decoder[Gte]       = io.circe.generic.semiauto.deriveDecoder[Gte]
}

case class Lt(lt: Json) extends ComparablePredicate(lt, _ < _, _ < _) {
  override def json: Json = this.asJson
}
object Lt {
  implicit val encoder: ObjectEncoder[Lt] = io.circe.generic.semiauto.deriveEncoder[Lt]
  implicit val decoder: Decoder[Lt]       = io.circe.generic.semiauto.deriveDecoder[Lt]
}

case class Lte(lte: Json) extends ComparablePredicate(lte, _ <= _, _ <= _) {
  override def json: Json = this.asJson
}
object Lte {
  implicit val encoder: ObjectEncoder[Lte] = io.circe.generic.semiauto.deriveEncoder[Lte]
  implicit val decoder: Decoder[Lte]       = io.circe.generic.semiauto.deriveDecoder[Lte]
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
object TestPredicate {
  implicit val encoder: ObjectEncoder[TestPredicate] = io.circe.generic.semiauto.deriveEncoder[TestPredicate]
  implicit val decoder: Decoder[TestPredicate]       = io.circe.generic.semiauto.deriveDecoder[TestPredicate]
}
