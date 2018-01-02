package week4

import java.util.NoSuchElementException

import sets.IntSet

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

object List {
  def apply[T](): List[T] = Nil
  def apply[T](one: T): List[T] = new Cons(one, Nil)
  def apply[T](one: T, two: T): List[T] = new Cons(one, new Cons(two, Nil))
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object MyApp extends App {
  def singleton[T](elem: T) = new Cons[T](elem, Nil)

  def nth[T](list: List[T], pos: Int): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    pos match {
      case p if p == 0 => list.head
      case _ => nth(list.tail, pos - 1)
    }
  }

  val lst = new Cons(1, new Cons(2, new Cons(3, new Cons(4, Nil))))

  println(nth(lst, -2))

  import sets.{Empty, NonEmpty}

  def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs prepend x

}