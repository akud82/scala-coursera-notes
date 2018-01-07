package week6

class Poly(termsInit: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms: Map[Int, Double] = termsInit.withDefaultValue(0d)

  def addTerm(ts: Map[Int, Double], t: (Int, Double)): Map[Int, Double] =
    ts + (t._1 -> (t._2 + ts.getOrElse(t._1, 0d)))

  def ++(other: Poly): Poly =
    new Poly((other.terms foldLeft terms) ((acc, v) => {
      acc + (v._1 -> (v._2 + acc.getOrElse(v._1, 0d)))
    }))

  def +(other: Poly): Poly = new Poly(
    this.terms ++ other.terms.map {
      case (k, v) => k -> (v + terms(k))
    }
  )

  override def toString: String =
    (for ((exp, coeff) <- terms.toList.sorted.reverse)
      yield exp match {
        case 0 => coeff
        case 1 => coeff + "x"
        case _ => coeff + "x^" + exp
      }).mkString(" + ")
}

object ex64 extends App {
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

  assert((p1 + p2).toString == (p1 ++ p2).toString,
    "fail:" + (p1 + p2).toString + "==" + (p1 ++ p2).toString)

  println("success")

  val m1 = Map(1 -> 1.0, 3 -> 3.0, 5 -> 5.2)
  val m2 = Map(0 -> 10.0, 3 -> 3.0)
  val merged = (m2 foldLeft m1) (
    (acc, v) => acc + (v._1 -> (v._2 + acc.getOrElse(v._1, 0.0)))
  )
  println(merged)
}
