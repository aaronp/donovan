package donovan.json
import io.circe.Decoder.Result
import io.circe._

/** Represents the operator which should be used when evaluating values within the same json document
  *
  * @param name the operator name
  */
sealed abstract class Op(val name: String) {

  def eval(lhs: Json, rhs: Json): Boolean
}

object Op {

  implicit object Format extends Encoder[Op] with Decoder[Op] {
    override def apply(a: Op): Json = Json.fromString(a.name)
    override def apply(c: HCursor): Result[Op] = {
      // flatMap is only available from 2.12, and we still support 2.11. Good 'ol Spark...
      c.as[String] match {
        case Right(key) =>
          val upperKey = key.toUpperCase
          Values.find(_.name == upperKey) match {
            case Some(found) => Right(found)
            case None        => Left(DecodingFailure(Values.mkString("Expected one of '", "', '", s"' but got '${key}'"), c.history))
          }
        case Left(err) => Left(err)
      }
    }
  }

  lazy val Values = Set(
    Equals,
    LT,
    LTE,
    GT,
    GTE,
    Before,
    After
  )

  case object Equals extends Op("EQ") {
    override def eval(lhs: Json, rhs: Json) = {
      donovan.json.Eq(rhs).matches(lhs)
    }
  }
  case object Before extends Op("BEFORE") {
    override def eval(lhs: Json, rhs: Json): Boolean = {
      // .exists on Either is only available from 1.12
      rhs.as[String] match {
        case Left(_)              => false
        case Right(referenceTime) => donovan.json.Before(referenceTime).matches(lhs)
      }
    }
  }
  case object After extends Op("AFTER") {
    override def eval(lhs: Json, rhs: Json): Boolean = {
      rhs.as[String] match {
        case Right(referenceTime) => donovan.json.After(referenceTime).matches(lhs)
        case Left(_) => false
      }
    }
  }
  case object GT extends Op("GT") {
    override def eval(lhs: Json, rhs: Json): Boolean = donovan.json.Gt(rhs).matches(lhs)
  }
  case object GTE extends Op("GTE") {
    override def eval(lhs: Json, rhs: Json): Boolean = donovan.json.Gte(rhs).matches(lhs)
  }
  case object LT extends Op("LT") {
    override def eval(lhs: Json, rhs: Json): Boolean = donovan.json.Lt(rhs).matches(lhs)
  }
  case object LTE extends Op("LTE") {
    override def eval(lhs: Json, rhs: Json): Boolean = donovan.json.Lte(rhs).matches(lhs)
  }
}
