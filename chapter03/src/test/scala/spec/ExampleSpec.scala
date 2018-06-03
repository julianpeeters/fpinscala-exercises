package spec

import example._

import org.specs2._

class ExampleSpec extends mutable.Specification {

  "3.1 - List(1,2,3,4,5)" should {
    "match Cons and Nil correctly" in {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
      x === 3
    }
  }

  "3.2 - List.tail(_)" should {
    "drop the first element" in {
      val ts = List(1,2,3,4,5)
      val r = List.tail(ts)
      r === List(2,3,4,5)
    }
  }

  "3.3 - List.setHead(_,_)" should {
    "replace the head element" in {
      val ts = List(1,2,3,4,5)
      val r = List.setHead(ts, 6)
      r === List(6,2,3,4,5)
    }
  }

  "3.4 - List.drop(_,n)" should {
    "remove the first n elements from a list" in {
      val ts = List(1,2,3,4,5)
      val r = List.drop(ts, 3)
      r === List(4,5)
    }
  }

  "3.5 - List.dropWhile(_,f)" should {
    "remove the first n elements that satisfy the function f from a list" in {
      val ts = List(1,2,3,4,5)
      val f: Int => Boolean = (a: Int) => a <= 3
      val r = List.dropWhile(ts,f)
      r === List(4,5)
    }
  }

  "3.6 - List.init(_)" should {
    "remove the last element from a list" in {
      val ts = List(1,2,3,4,5)
      val r = List.init(ts)
      r === List(1,2,3,4)
    }
  }


  //"3.7 - How can product, implemented with foldRight, short circuit if a 0 is encountered?" yes, but only with a custom foldRight
  //"3.8 - args to fold right and the data constructors of List are isomorphic, binary functions"

  "3.9 - List.length(_)" should {
    "find the length of a list using foldRight" in {
      val ts = List(1,2,3,4,5)
      val r = List.length(ts)
      r === 5
    }
  }

  "3.10 - List.foldLeft(_)(_)" should {
    "find the length of a list using foldLeft" in {
      val ts = List(1,2,3,4,5)
      val r = List.foldLeft(ts,1)(_ * _)
      r === 120
    }
  }

  "3.11 - List" should {
    "find a sum using foldLeft" in {
      val ts = List(1,2,3,4,5)
      val r = List.sumLeft(ts)
      r === 15
    }
    "find a product using foldLeft" in {
      val ts = List(1,2,3,4,5)
      val r = List.productLeft(ts)
      r === 120
    }
    "find a length using foldLeft" in {
      val ts = List(1,2,3,4,5)
      val r = List.lengthLeft(ts)
      r === 5
    }
  }

  "3.12 - List.reverse(_)" should {
    "find the reverse of a list" in {
      val ts = List(1,2,3,4,5)
      val r = List.reverse(ts)
      r === List(5,4,3,2,1)
    }
  }

  "3.13 - List.foldLeft(_)(_)" should {
    "implement foldLeft using foldRight" in {
      val ts = List(4,5)
      val r = List.foldLeftViaRight(ts,20.0)(_ / _)
      r === 1
    }
    "implement foldLeft using foldRight by accumulating a function" in {
      val ts = List(4,5)
      val r = List.foldLeftViaRight_1(ts,20.0)(_ / _)
      r === 1
    }
    "implement foldRight using foldLeft" in {
      val ts = List(12.0,18.0)
      def fr[A,B](l: List[A], z: B)(f: (A,B)=> B): B = {
        List.foldLeft(List.reverse(l), z)((b, a) => f(a, b))
      }
      val r = fr(ts,3.0)(_ / _)
      r === 2.0
    }
    "implement foldRight using foldLeft by accumulating a function" in {
      val ts = List(12.0,18.0)
      val r = List.foldRightViaLeft_1(ts,3.0)(_ / _)
      r === 2.0
    }
  }


  "3.14 - List" should {
    "append using foldLeft" in {
      val ts = List(1,2,3,4,5)
      val r = List.appendViaFoldLeft(ts,6)
      r === List(1,2,3,4,5,6)
    }
    "append using foldRight" in {
      val ts = List(1,2,3)
      val r = List.appendViaFoldRight(ts,4)
      r === List(1,2,3,4)
    }
  }

  "3.15 - List" should {
    "concat two List" in {
      val ts = List(1,2)
      val us = List(3,4,5,6)
      val r = List.concat(ts,us)
      r === List(1,2,3,4,5,6)
    }
  }

  "3.16 - List" should {
    "addOne to each element" in {
      val us = List(3,4,5,6)
      val r = List.addOne(us)
      r === List(4,5,6,7)
    }
  }

  "3.17 - List" should {
    "toString on each element" in {
      val us = List(3,4,5,6)
      val r = List.toString(us)
      r === List("3","4","5","6")
    }
  }

  "3.18 - List" should {
    "map on each element" in {
      val us = List(3,4,5,6)
      val f: Int => String = u => u.toString
      val r = List.map(us, f)
      r === List("3","4","5","6")
    }
  }

  "3.19 - List" should {
    "filter" in {
      val us = List(3,4,5,6)
      val f: Int => Boolean = u => u > 4
      val r = List.filter(us, f)
      r === List(5,6)
    }
  }

  "3.20 - List" should {
    "filter" in {
      val us = List(1,2,3)
      val f: Int => List[Int] = i => List(i,i)
      val r = List.flatMap(us)(f)
      r === List(1,1,2,2,3,3)
    }
  }

  "3.21 - List" should {
    "filterViaFlatMap" in {
      val us = List(3,4,5,6)
      val f: Int => Boolean = u => u > 4
      val r = List.filterViaFlatMap(us, f)
      r === List(5,6)
    }
  }

  "3.22 - List" should {
    "zipAdd" in {
      val a1s = List(1,2,3)
      val a2s = List(4,5,6)
      val r = List.zipAdd(a1s, a2s)
      r === List(5,7,9)
    }
  }

  "3.23 - List" should {
    "zipWith" in {
      val a1s = List(1,2,3)
      val a2s = List(4,5,6)
      val r = List.zipWith(a1s, a2s)(_ + _)
      r === List(5,7,9)
    }
  }

  "3.24 - List" should {
    "hasSubsequence" in {
      val a1s = List(1,2,3,4,5,6)
      val a2s = List(3,4,5)
      val r = List.hasSubsequence(a1s, a2s)
      r === true
    }
    "hasSubsequenceViaTake" in {
      val a1s = List(1,2,3,4,5,6)
      val a2s = List(3,4,5)
      val r = List.hasSubsequenceViaTake(a1s, a2s)
      r === true
    }
  }

  "3.25 - Tree" should {
    "size" in {
      val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      val r = Tree.size(t)
      r === 2
    }
  }

  "3.26 - Tree" should {
    "maximum" in {
      val t = Branch(Branch(Branch(Leaf(10), Leaf(2)), Leaf(6)), Leaf(3))
      val r = Tree.maximum(t)
      r === 10
    }
  }

  "3.27 - Tree" should {
    "depth" in {
      val t = Branch(Branch(Leaf(1), Leaf(6)), Leaf(3))
      val r = Tree.depth(t)
      r === 2
    }
  }

  "3.28 - Tree" should {
    "map" in {
      val t = Branch(Branch(Leaf(1), Leaf(6)), Leaf(3))
      val r = Tree.map(t)(_ + 1)
      r === Branch(Branch(Leaf(2), Leaf(7)), Leaf(4))
    }
  }

  "3.29 - Tree" should {
    "fold" in {
      val t = Branch(Branch(Leaf(1), Leaf(6)), Leaf(3))
      val r = Tree.fold(t)(_ + 10)(_ + _)
      r === 40
    }
  }
}