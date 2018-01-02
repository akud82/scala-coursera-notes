package idealized.scala

abstract class _boolean {
  def ifThenElse[T](t: => T, e: => T): T

  def &&(x: => Boolean): Boolean = ifThenElse(x, false)

  def ||(x: => Boolean): Boolean = ifThenElse(true, x)

  def unary_!(): Boolean = ifThenElse(false, true)

  def <(x: => Boolean): Boolean = ifThenElse(false, x)
}

object _true extends _boolean {
  def ifThenElse[T](t: => T, e: => T): T = t
}

object _false extends _boolean {
  def ifThenElse[T](t: => T, e: => T): T = e
}

object main extends App {

}