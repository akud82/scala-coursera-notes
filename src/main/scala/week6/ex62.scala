package week6
import scala.collection.immutable._

object pairs extends App {
  def isPrime(n: Int): Boolean = 2.until(n).forall(n % _ != 0)

  def gen(n: Int) = {
    (1 until n).flatMap(i =>
      (1 until i).map(j => (i, j))
    ).filter({
      case (l, r) => isPrime(l + r)
    }).map({
      case (l, r) => (l, r, l + r)
    })
  }

  def gen2(n: Int) = {
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j, i + j)
  }

  println(
    gen2(7)
  )
}

object combEx extends App {
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Vector[Double] =
    (xs zip ys).map {
      case (x, y) => x * y
    }

  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Vector[Double] =
    for {
      (x, y) <- xs zip ys
    } yield {
      x * y
    }

  println(
    scalarProduct2(Vector(1, 2, 3), Vector(3, 4, 5))
  )
}

object ex62 extends App {

}
