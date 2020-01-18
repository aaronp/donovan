package donovan.core

trait DataDiff[A, D] {

  def diff(lhs: A, rhs: A): D

  def map[D1](f: D => D1): DataDiff[A, D1] = {
    val self = this
    new DataDiff[A, D1] {
      override def diff(lhs: A, rhs: A) = f(self.diff(lhs, rhs))
    }
  }
}
