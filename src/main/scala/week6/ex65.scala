package week6
import scala.io.Source

object ex65 extends App {
  val in = Source.fromURL("file:///Users/alexeus/words/linuxwords.txt")
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

  val mnemonics = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case a :: _ =>
      val (f, s) = xs span (y => y == a)
      f :: pack(s)
  }

  def charCode: Map[Char, Char] =
    for {
      (digit, str) <- mnemonics
      ltr <- str
    } yield ltr -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode

  // "5282" -> List("JAVA", "LAVA", "KATA")
  // Note: a missing number should map to the empty set, e.g. "1111" -> List()
  val wordsForNum: Map[String, Seq[String]] = (words groupBy wordCode) withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def translate(phoneNumber: String): Set[String] =
    encode(phoneNumber) map (_ mkString " ")

  val phoneNumber = "7225247386"

  println(
    translate(phoneNumber)
  )
}