package donovan.core

/**
  * Represents something which can produce a difference 'D' between a left-hand-side and right-hand-side of 'A'
  *
  * @tparam A
  * @tparam D
  */
trait DataDiff[A, D] {

  /** @param lhs
    * @param rhs
    * @return the difference between lhs and rhs
    */
  def diff(lhs: A, rhs: A): D

  def map[D1](f: D => D1): DataDiff[A, D1] = {
    val self = this
    new DataDiff[A, D1] {
      override def diff(lhs: A, rhs: A) = f(self.diff(lhs, rhs))
    }
  }
}
