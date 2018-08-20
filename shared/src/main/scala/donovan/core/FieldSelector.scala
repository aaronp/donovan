package donovan.core

trait FieldSelector[A, B] {

  def select(in: A): B
}

object FieldSelector {

  def lift[A, B](f: A => B): FieldSelector[A, B] = {
    new FieldSelector[A, B] {
      override def select(in: A) = f(in)
    }
  }
}
