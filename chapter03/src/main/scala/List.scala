package example

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def addOne(us: List[Int]): List[Int] =
    foldRight[Int, List[Int]](us, Nil)((u,z) => Cons(u + 1, z))

  def appendViaFoldLeft[A](as: List[A], z: A): List[A] = {
    List.foldLeft(List.reverse(as), Cons(z, Nil))((b,a) => Cons(a, b))
  }

  def appendViaFoldRight[A](as: List[A], z: A): List[A] = {
    List.foldRight(as, Cons(z, Nil))((a, b) => Cons(a,b))
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
  }

  def concat[T](ts: List[T], ss: List[T]): List[T] = {
    List.foldRight(ts, ss)((a,b) => Cons(a, b))
  }

  def drop[T](ts: List[T], n: Int): List[T] = {
    ts match {
      case Nil => ts
      case Cons(h,t) => if (n==0) ts else drop(t, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case cons@Cons(h,t) => if (f(h)) dropWhile(t,f) else cons
    }
  }

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((a, z) => if (f(a)) Cons(a, z) else z)

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight[A, List[B]](as, Nil)((a,z) => concat(f(a), z))

  // RE folds: recurse, passing the accumulator along
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }
  }

  def foldLeftViaRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a,b)=>f(b,a))
  }

  def foldLeftViaRight_1[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    val fAcc = foldRight(as, (b:B)=>b)((a,c)=>b=>c(f(b,a)))
    fAcc(z) // build up a function, then apply it
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B)=>B): B = {
    as match {
      case Nil => z
      case Cons(h,t) => f(h, foldRight(t,z)(f))
    }
  }

  def foldRightViaLeft[A,B](as: List[A], z: B)(f: (A,B)=>B): B = {
    foldLeft(reverse(as), z)((b, a)=>f(a,b))
  }

  def foldRightViaLeft_1[A,B](as: List[A], z: B)(f: (A,B)=>B): B = {
    val fAcc = foldLeft(as, (b:B) => b)((c,a)=>b=>c(f(a,b)))
    fAcc(z) // build up a function, then apply it
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil       => false
    case fragment@Cons(h,t) =>
      val zippedWithEquals: List[Boolean] = zipWith(fragment, sub, Nil)((p: A, b: A) => p == b)
      val corresponds: Boolean = foldRight(zippedWithEquals, true)(_ && _)
      if (corresponds) true
      else hasSubsequence(t,sub)
  }

  def hasSubsequenceViaTake[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil       => false // no subsequences of sup to compare
    case Cons(h,t) =>
      if (take(sup, length(sub)) == sub) true
      else hasSubsequenceViaTake(t,sub)
  }

  def init[A](as: List[A]): List[A] = {
    as match {
      case Nil => as
      case Cons(u, Nil) => Nil
      case Cons(h,t) => Cons(h, init(t))
    }
  }

  def length[A](as: List[A]): Int = {
    foldRight(as,0)((_,n)=>n+1) // takes advantage of advancing to increment, but discards value
  }

  def lengthLeft(ints: List[Int]): Int = {
    foldLeft(ints,0)((n,_)=>n+1)
  }

  def map[A,B](us: List[A], f: A => B): List[B] =
    foldRight[A, List[B]](us, Nil)((u,z) => Cons(f(u), z))

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(h,t) => h * product(t)
    }
  }

  // def productRight(ds: List[Double]): Double = {
  //   ds.foldRight
  // }

  def productLeft(ints: List[Int]): Int = {
    foldLeft(ints,1)(_*_)
  }

  def reverse[A](ints: List[A]): List[A] = {
    @annotation.tailrec
    def iter(xs: List[A], acc: List[A]): List[A] = {
      xs match {
        case Nil => acc
        case Cons(h,t) => iter(t,Cons(h,acc))
      }
    }
    iter(ints, Nil)
  }

  def reverseLeft[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((acc, a)=>Cons(a,acc))
  }

  // def reverseRight[A](as: List[A]): List[A] = {
  //   foldRight(as, List[A]())((a, acc)=>Cons(a,acc))
  // }

  def setHead[T](ts: List[T], nh: T): List[T] = {
    ts match {
      case Nil => Cons(nh, Nil)
      case cons@Cons(h,t) => cons.copy(head = nh)
    }
  }

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(h,t) => h + sum(t)
    }
  }

  def sumLeft(ints: List[Int]): Int = {
    foldLeft(ints,0)(_+_)
  }

  def tail[T](ts: List[T]): List[T] = {
    ts match {
      case Nil => ts
      case Cons(h,t) => t
    }
  }

  def take[T](ts: List[T], n: Int, zs: List[T] = Nil): List[T] = {
    ts match {
      case Nil => zs
      case Cons(h,t) => if (n==0) zs else take(t, n-1, appendViaFoldRight(zs,h))
    }
  }

  def toString[A,B](us: List[A]): List[String] = {
    foldRight[A, List[String]](us, Nil)((u,z) => Cons(u.toString, z))
  }

  def zipAdd(a1s: List[Int], a2s: List[Int], zs: List[Int] = Nil): List[Int] = {
    (a1s, a2s) match {
      case (Nil,         Nil)         => reverse(zs)
      case (Nil,         Cons(h2,t2)) => reverse(zs)
      case (Cons(h1,t1), Nil)         => reverse(zs)
      case (Cons(h1,t1), Cons(h2,t2)) => zipAdd(t1, t2, Cons(h1+h2,zs))
    }
  }

  def zipWith[A,B](a1s: List[A], a2s: List[A], zs: List[B] = Nil)(f: (A,A) => B): List[B] = {
    (a1s, a2s) match {
      case (Nil,         Nil)         => reverse(zs)
      case (Nil,         Cons(h2,t2)) => reverse(zs)
      case (Cons(h1,t1), Nil)         => reverse(zs)
      case (Cons(h1,t1), Cons(h2,t2)) => zipWith(t1, t2, Cons(f(h1,h2),zs))(f)
    }
  }

}