package example

trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f(_)).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) Some(a) else None)
}

object Option {

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      i <- a
      j <- b
    } yield f(i,j)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil       => Some(Nil)
      case Cons(h,t) => h.flatMap(ha => sequence(t).map(tas => Cons(ha,tas)))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil       => Some(Nil)
      case Cons(h,t) => traverse(t)(f).flatMap(tas => f(h).map(a => Cons(a,tas)))
    }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]