package week41
import scala.concurrent.Future

object ListUtils {
  def last[T](xs: List[T]): T =
    xs match {
      case List() => throw new Error("empty")
      case List(x) => x
      case _ :: ys => last[T](ys)
    }

  def init[T](xs: List[T]): List[T] =
    xs match {
      case List() => throw new Error("empty")
      case List(_) => List()
      case y :: ys => y :: init(ys)
    }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case y :: ys => y match {
      case z: List[Any] => flatten(z) ++ flatten(ys)
      case _ => y :: flatten(ys)
    }
  }
}

object app extends App {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (x <= y) x :: xs
      else y :: insert(x, ys)
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  object mergesort {
    def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[T], ys: List[T]): List[T] = {
          (xs, ys) match {
            case (Nil, bs) => bs
            case (as, Nil) => as
            case (a :: as, b :: bs) =>
              if (ord.lt(a, b)) a :: merge(as, ys)
              else b :: merge(xs, bs)
          }
        }

        val (f, s) = xs splitAt n
        merge(msort(f), msort(s))
      }
    }
  }

  import mergesort._

  val nums = List(145, 2, 43, 4, 100, 102, 5, 6, 7, 8, 9)
  val fruits = List("fruit", "apple", "pineapple", "banana")

  println(msort(nums))
  println(msort(fruits))
}
