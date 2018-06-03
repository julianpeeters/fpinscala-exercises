package example

object Composer {

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    //f.compose(g)
    (a: A) => f(g(a))
  }

}
