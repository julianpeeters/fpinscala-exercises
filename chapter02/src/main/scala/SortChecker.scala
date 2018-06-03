package example

object SortChecker {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as.forall(a => {
      val idx = as.indexOf(a)
      if (idx == 0) true
      else ordered(as(idx-1), a)
    })
  }
}