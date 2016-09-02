package nl.jqno.fpscala.ch4_handling_errors

import EitherFunctions._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.control.NonFatal

class EitherTest extends FlatSpec with Matchers {
  val right: Either[String, Int] = Right(1)
  val right2: Either[String, Int] = Right(2)
  val originalLeft: Either[String, Int] = Left("original left")
  val oddLeft = Left("odd")

  val even = (i: Int) => i % 2 == 0
  val evenOpt = (i: Int) => if (even(i)) Right(i) else oddLeft
  val parseInt = (s: String) => try { Right(s.toInt) } catch { case NonFatal(_) => originalLeft }

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


  behavior of "sequence"

  it should "return Right(List) when all input values are also Right" in {
    sequence(List(Right(1), Right(2), Right(3))) should be (Right(List(1, 2, 3)))
  }

  it should "return None if any of its input values is also None" in {
    sequence(List(Right(1), originalLeft, Right(3))) should be (originalLeft)
    sequence(List(originalLeft, Right(2), Right(3))) should be (originalLeft)
  }


  behavior of "traverse"

  it should "return a Some of a List of Ints if all Strings can be parsed" in {
    traverse(List("1", "2", "3"))(parseInt) should be (Right(List(1, 2, 3)))
  }

  it should "return a None if one of the Strings can't be parsed" in {
    traverse(List("1", "refridgerator", "3"))(parseInt) should be (originalLeft)
  }
}
