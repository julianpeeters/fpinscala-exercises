package example

object Fib {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibonize(n: Int, seq: Seq[Int]): Seq[Int] = {
      n match {
        case 1 =>
          seq
        case _ =>
          fibonize(n-1, seq:+(seq.takeRight(2).sum))
      }
    }
    fibonize(n, Seq(0, 1)).last
  }
  
}