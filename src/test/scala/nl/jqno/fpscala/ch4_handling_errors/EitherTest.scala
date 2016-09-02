package nl.jqno.fpscala.ch4_handling_errors

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {
  val right: Either[String, Int] = Right(1)
  val right2: Either[String, Int] = Right(2)
  val originalLeft: Either[String, Int] = Left("original left")
  val oddLeft = Left("odd")

  val even = (i: Int) => i % 2 == 0
  val evenOpt = (i: Int) => if (even(i)) Right(i) else oddLeft

  behavior of "map"

  it should "map a Right" in {
    right.map(_ + 1) should be (Right(2))
  }

  it should "map a Left" in {
    originalLeft.map(_ + 1) should be (originalLeft)
  }


  behavior of "flatMap"

  it should "flatMap a Right" in {
    right2.flatMap(evenOpt) should be (Right(2))
    right.flatMap(evenOpt) should be (oddLeft)
  }

  it should "flatMap a Left" in {
    originalLeft.flatMap(evenOpt) should be (originalLeft)
  }


  behavior of "orElse"

  it should "return itself if it's a Right" in {
    right.orElse(right2) should be (right)
  }

  it should "return the default value if it's a Left" in {
    originalLeft.orElse(right2) should be (right2)
  }


  behavior of "map2"

  it should "return a Right with the correct value when the inputs are both Right" in {
    right.map2(right2)(_ + _) should be (Right(3))
  }

  it should "return Left when either input is Left" in {
    originalLeft.map2(right)(_ + _) should be (originalLeft)
    right.map2(originalLeft)(_ + _) should be (originalLeft)
  }
}
