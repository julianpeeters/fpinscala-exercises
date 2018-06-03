package spec

import example._

import org.specs2._

class ExampleSpec extends mutable.Specification {

  "fib" should {
    "Calculate the nth term of the Fibonacci sequence" in {
      Fib.fib(4) === 3
    }
  }

  "isSorted" should {
    "Sort an array of any type for which be provide a sort function" in {
      val myExampleArray = Array(1,7,3,4)
      val mySortFunction = (a: Int, b: Int) => {
        if (a > b) false
        else true
      }
      SortChecker.isSorted(myExampleArray, mySortFunction) === false
    }
  }

  "curry" should {
    "curry a curryable function" in {
      val myCurryableFunction = (a: Int, b: Int) => a + b
      Currier.curry(myCurryableFunction)(1)(2) === 3
    }
  }

  "uncurry" should {
    "uncurry a uncurryable function" in {
      val myUncurryableFunction = (a: Int) => ((b: Int) => a + b)
      Currier.uncurry(myUncurryableFunction)(1,2) === 3
    }
  }

  "compose" should {
    "compose two function f and g to produce f of g" in {
      val f: Boolean => Int = bool => if (bool) 1 else 0
      val g: String => Boolean = str => str.toBoolean
      Composer.compose(f, g).apply("true") === 1
    }
  }

}