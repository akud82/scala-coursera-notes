package week5

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