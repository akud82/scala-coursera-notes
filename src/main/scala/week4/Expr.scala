package week4

import sets.{IntSet, NonEmpty, Empty}

class Fn1[-T, +U] {
  def apply(x: T): U = ???
}

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object ex01 extends App {
  def eval(e: Expr): Int = e match {
    case Number(x) => x
    case Prod(e1, e2) => eval(e1) * eval(e2)
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(x) => s"$x"
    case Prod(e1, e2) =>
      def parenth(e: Expr): String = e match {
        case Sum(_, _) => s"(${show(e)})"
        case _ => s"${show(e)}"
      }

      s"${parenth(e1)} * ${parenth(e2)}"

    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Var(name) => s"$name"
  }

  implicit def int2num(i: Int): Number = Number(i)

  val p1 = Prod(Sum(2, Var("x")), Var("y"))
  val p2 = Sum(Prod(2, Var("x")), Var("y"))
  val p3 = Prod(Sum(2, Var("x")), Sum(2, Var("x")))
  val p4 = Prod(Var("y"), Sum(2, Var("x")))

  println(show(p1))
  println(show(p2))
  println(show(p3))
  println(show(p4))
}
