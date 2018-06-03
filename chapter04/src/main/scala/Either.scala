package example

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l@Left(e) => l
    case Right(a)  => Right(f(a))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l@Left(e) => l
    case Right(a) => f(a)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case r@Right(a) => r
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      i <- this
      j <- b
    } yield f(i,j)
}
object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Cons(he,tes) => he.map2(sequence(tes))(Cons(_,_))
  }
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case Cons(h,t) => f(h).map2(traverse(t)(f))(Cons(_,_))
  }
  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

case class GenericError