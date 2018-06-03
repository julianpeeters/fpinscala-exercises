package spec

import example._

import org.specs2._

class ExampleSpec extends mutable.Specification {

  "4.1 - Option" should {
    "map" in {
      val x = Some(1).map(_ + 1)
      x === Some(2)
    }
    "flatMap" in {
      val x = Some(1).flatMap(Some(_))
      x === Some(1)
    }
    "getOrElse" in {
      val x = Some(3).getOrElse(9)
      x === 3
    }
    "orElse" in {
      val x = None.orElse(Some(5))
      x === Some(5)
    }
    "filter" in {
      val x = Some(1).filter(_ != 1)
      x === None
    }
  }

  "4.2 - Option" should {
    "variance" in {
      val x = Stats.variance(Seq(2.0, 4.0, 6.0, 8.0))
      x === Some(5.0)
    }
  }

  "4.3 - Option" should {
    "map2" in {
      val x = Option.map2(Some(1), Some(2))(_ + _)
      x === Some(3)
    }
  }


  "4.4 - Option" should {
    "sequence" in {
      val x = Option.sequence(List(Some(2.0), Some(4.0), Some(6.0), Some(8.0)))
      x === Some(List(2.0, 4.0, 6.0, 8.0))
    }
  }

  "4.5 - Option" should {
    "traverse" in {
      val x = Option.traverse(List(2.0, 4.0, 6.0, 8.0))(x => (Some(x)))
      x === Some(List(2.0, 4.0, 6.0, 8.0))
    }
    "sequenceViaTraverse" in {
      val x = Option.sequenceViaTraverse(List(Some(2.0), Some(4.0), Some(6.0), Some(8.0)))
      x === Some(List(2.0, 4.0, 6.0, 8.0))
    }
  }

  "4.6 - Either" should {
    "map" in {
      val x = Right(1).map(_ + 1)
      x === Right(2)
    }
    "flatMap" in {
      val x = Right(1).flatMap(Right(_))
      x === Right(1)
    }
    "orElse" in {
      val x = Left(GenericError).orElse(Right(5))
      x === Right(5)
    }
    "map2" in {
      val x = Right(1).map2(Right(5))(_ + _)
      x === Right(6)
    }
  }

  "4.7 - Either" should {
    "sequence" in {
      val x = Either.sequence(List(Right(2.0), Right(4.0), Right(6.0), Right(8.0)))
      x === Right(List(2.0, 4.0, 6.0, 8.0))
    }
    "traverse" in {
      val x = Either.traverse(List(2.0, 4.0, 6.0, 8.0))(Right(_))
      x === Right(List(2.0, 4.0, 6.0, 8.0))
    }
    "sequenceViaTraverse" in {
      val x = Either.sequenceViaTraverse(List(Right(2.0), Right(4.0), Right(6.0), Right(8.0)))
      x === Right(List(2.0, 4.0, 6.0, 8.0))
    }
  }

   // "4.8 - Either":
   //   - Use Either[List[E], A]??
   //   - Applicative (e.g. Validation, with it's own map2)??
   //   - getOrElse, traverse, sequence, all behave the same with an Applicative
 
}