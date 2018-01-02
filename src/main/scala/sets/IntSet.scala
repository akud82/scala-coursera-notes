package sets

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = NonEmpty(x, new Empty, new Empty)

  override def toString: String = "."

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean =
    x match {
      case i if i < elem => left.contains(i)
      case i if i > elem => right.contains(i)
      case _ => true
    }

  override def incl(x: Int): IntSet =
    x match {
      case i if i < elem => NonEmpty(elem, left.incl(i), right)
      case i if i > elem => NonEmpty(elem, left, right.incl(i))
      case _ => this
    }

  override def toString: String = s"[$left$elem$right]"

  override def union(other: IntSet): IntSet = {
    println(s"union [left=$left, right=$right, elem=$elem, other=$other]")
    left.union(right).union(other).incl(elem)
  }
}

object NonEmpty {
  def apply(elem: Int, left: IntSet, right: IntSet): NonEmpty = new NonEmpty(elem, left, right)
}

object Main extends App {
  val empty = new Empty()
  val s1 = empty.incl(1).incl(3).incl(5).incl(9).incl(13)
    .incl(10).incl(7).incl(10).incl(5).incl(100).incl(53)
    .incl(11).incl(23).incl(-1).incl(6)

  val s2 = empty.incl(11).incl(43).incl(5).incl(9).incl(13)
    .incl(10).incl(7).incl(10).incl(51).incl(100).incl(53)
    .incl(11).incl(23).incl(-13).incl(641)

  println(s1.union(s2))
}
