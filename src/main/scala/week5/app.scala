package week5

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
    println((a, b, c))
  }
}

object week5 extends App {
  import mergesort._

  val nums = List(145, 2, 43, 4, 100, 102, 5, 6, 7, 8, 9)
  val fruits = List("fruit", "apple", "pineapple", "banana")

  println(msort(nums))
  println(msort(fruits))
}
