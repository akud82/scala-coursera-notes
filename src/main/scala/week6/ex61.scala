package week6

object ex61 extends App {
  val s = "Hello World"
  val pairs = 1.to(Int.MaxValue) zip s


  //  println(
  //    pairs
  //  )
  //
  //  println(
  //    pairs.unzip
  //  )

  //  println(
  //    s.flatMap(c => c +: '1'.to('4') :+ c)
  //  )

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Vector[Double] =
    (xs zip ys).map {
      case (x, y) => x * y
    }

  // Seq :> Vector, Seq :> List

  def isPrime(n: Int): Boolean = 2.until(n).forall(n % _ != 0)

  println(
    scalarProduct(Vector(1, 2, 3), Vector(3, 4, 5))
  )

  println(
    1.to(1000).flatMap { x =>
      if (isPrime(x)) Vector(x)
      else Vector()
    }
  )
}
