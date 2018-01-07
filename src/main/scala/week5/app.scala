package week5
import scala.collection.immutable
import scala.concurrent.Future

object triangles extends App {
  // a,b - cathetus, c - hypotenuse
  // b is a long cathetus, a - short
  // return all triangles with perimeter 24

  // Haskell:
  // `[(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b], b < 30, a^2 + b^2 == c^2, a + b + c == 24]`
  for {
    c <- 1 to 1000
    b <- 1 to c
    a <- 1 to b
    if (a * a) + (b * b) == (c * c)
    if a + b + c == 24
    if b < 30
  } yield {
    (a, b, c)
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  val withMaxSrc = (maxVal: Int) => Future {
    Thread.sleep(500)
    for {b <- 1 to maxVal} yield b
  }

  val res = for {
    c <- withMaxSrc(1000)
    b <- withMaxSrc(300)
    a <- withMaxSrc(300)
  } yield {
    for {
      cVal <- c
      bVal <- b
      aVal <- a
      if (aVal * aVal) + (bVal * bVal) == (cVal * cVal)
      if aVal + bVal + cVal == 24
      if bVal < 30
    } yield {
      (aVal, bVal, cVal)
    }
  }

  println(res.value)

}

object listfun {
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case a :: _ =>
      val (f, s) = xs span (y => y == a)
      f :: pack(s)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs).map((a) => (a.head, a.length))
}

object ex54 extends App {

  val nums = List(1, -4, 24, 52, 5, -55)

  nums.filter(x => x > 0)
  nums.filterNot(x => x > 0)
  nums.partition(x => x > 0)
  nums.span(x => x > 0)

  import listfun._

  val data = List("a", "a", "a", "b", "c", "c", "a")
  println(
    pack(data)
  )

  println(
    encode(data)
  )
}

object ex55 extends App {
  def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)
  def product(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _)

  def sum2(xs: List[Int]): Int = (xs foldLeft 0) (_ + _)
  def product2(xs: List[Int]): Int = (xs foldLeft 1) (_ * _)

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) ((x, y) => x :: y)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((t, acc) => f(t) :: acc)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((_, acc) => acc + 1)

  val l = List(1, 2, 3, 4, 5)
  println(
    mapFun(l, (x: Int) => "[" + x.toString + "]")
  )

  println(
    lengthFun(l)
  )
}

object ex56 extends App {
  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)

  def power(n: Int): Double = Math.pow(2, n)

  val n = 7
  println(
    power(n + 1)
  )
  println(
    factorial(n)
  )
}

object week5 extends App {

}
