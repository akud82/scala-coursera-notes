package week6

object nqueens extends App {
  def isSafeColumn(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens // List((3, 2), (2, 0), (1, 3), (0, 1))
    queensWithRow forall {
      case (r, colPos) => col != colPos && math.abs(col - colPos) != row - r
    }
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(row: Int): Set[List[Int]] =
      if (row == 0) Set(List())
      else
        for {
          queens <- placeQueens(row - 1)
          col <- 0 until n
          if isSafeColumn(col, queens)
        } yield col :: queens

    placeQueens(n)
  }

  def show(queens: List[Int]): String = {
    val queensPositions = (queens.reverse zip queens.indices).toSet
    val ANSI_BLACK_BACKGROUND = "\u001B[40m"
    val ANSI_WHITE = "\u001B[37m"
    val ANSI_RESET = "\u001B[0m"
    val QUEEN_SYMBOL = " ♛ "
    val EMPTY_SYMBOL = "   "

    (for (c <- queens.indices; r <- queens.indices) yield (r, c))
      .map({
        case (r, c) =>
          val pf =
            if ((r + c) % 2 == 0) s"$ANSI_BLACK_BACKGROUND$ANSI_WHITE"
            else s"$ANSI_RESET"

          val sf = s"$ANSI_RESET"

          if (queensPositions.contains((r, c))) pf + QUEEN_SYMBOL + sf
          else pf + EMPTY_SYMBOL + sf

      })
      .sliding(queens.length, queens.length)
      .toList
      .map(l => l.mkString)
      .mkString("\n")

  }

  val res = queens(6)
  println(
    res map ("\n" + show(_) + "\n")
  )

  println(
    s"Variants found: ${res.size}"
  )
}

object ex63 extends App {

}
