package example

object Stats {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(s: Seq[Double]) = xs match {
      case Seq() => None
      case nonEmpty => Some(nonEmpty.sum/nonEmpty.length)
    }
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
   }
}