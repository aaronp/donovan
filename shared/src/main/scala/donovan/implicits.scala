package donovan

import donovan.json.JExpression.LowPriorityJExpressionImplicits
import donovan.json.JPredicate.LowPriorityPredicateImplicits
import donovan.json.RichJsonOps
import donovan.json.RichJsonOps.LowPriorityJsonOpsImplicits
import donovan.time.TimeLowPriorityImplicits
import io.circe.java8.time.TimeInstances
import io.circe.{Decoder, Encoder, Json}

trait implicits extends TimeInstances with LowPriorityPredicateImplicits with LowPriorityJExpressionImplicits with LowPriorityJsonOpsImplicits with TimeLowPriorityImplicits {

  implicit def asRichJsonOps(json: Json) = RichJsonOps(json)

  implicit val ThrowableEncoder: Encoder[Throwable] = {
    Encoder.encodeString.contramap((e: Throwable) => e.getMessage)
  }
  implicit val ThrowableDecoder: Decoder[Throwable] = {
    Decoder.decodeString.map(err => new Exception(err))
  }
}

object implicits extends implicits
