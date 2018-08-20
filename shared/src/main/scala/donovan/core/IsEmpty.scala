package donovan.core
import io.circe.Json
import simulacrum.typeclass

@typeclass trait IsEmpty[-T] {
  def isEmpty(value: T): Boolean
  def nonEmpty(value: T): Boolean = !isEmpty(value)
}

object IsEmpty {

  implicit object SeqIsEmpty extends IsEmpty[TraversableOnce[_]] {
    override def isEmpty(value: TraversableOnce[_]): Boolean = value.isEmpty
  }
  implicit object ArrayIsEmpty extends IsEmpty[Array[_]] {
    override def isEmpty(value: Array[_]): Boolean = value.isEmpty
  }

  implicit object StrIsEmpty extends IsEmpty[String] {
    override def isEmpty(value: String): Boolean = value == null || value.isEmpty
  }
  implicit object JsonIsEmpty extends IsEmpty[Json] {
    override def isEmpty(value: Json): Boolean = {
      value.isNull || !value.hcursor.downField("deltas").values.exists(_.nonEmpty)
    }
  }

}
