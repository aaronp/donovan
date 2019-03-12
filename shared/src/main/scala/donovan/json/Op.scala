package donovan.json
import io.circe.Decoder.Result
import io.circe._

sealed abstract class Op(val name: String) {

  def eval(lhs: Json, rhs: Json): Boolean
}

object Op {

  implicit object Format extends Encoder[Op] with Decoder[Op] {
    override def apply(a: Op): Json = Json.fromString(a.name)
    override def apply(c: HCursor): Result[Op] = {
      c.as[String].flatMap { key =>
        val upperKey = key.toUpperCase
        Values.find(_.name == upperKey) match {
          case Some(found) => Right(found)
          case None        => Left(DecodingFailure(Values.mkString("Expected one of '", "', '", s"' but got '${key}'"), c.history))
        }
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
    After,
  )

  case object Equals extends Op("EQ") {
    override def eval(lhs: Json, rhs: Json) = {
      donovan.json.Eq(rhs).matches(lhs)
    }
  }
  case object Before extends Op("BEFORE") {
    override def eval(lhs: Json, rhs: Json): Boolean = {
      rhs.as[String].exists { referenceTime =>
        donovan.json.Before(referenceTime).matches(lhs)
      }
    }
  }
  case object After extends Op("AFTER") {
    override def eval(lhs: Json, rhs: Json): Boolean = {
      rhs.as[String].exists { referenceTime =>
        donovan.json.After(referenceTime).matches(lhs)
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
