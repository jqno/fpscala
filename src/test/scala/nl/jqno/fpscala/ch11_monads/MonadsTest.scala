package nl.jqno.fpscala.ch11_monads

import org.scalatest.{FlatSpec, Matchers}
import Monad._

class MonadsTest extends FlatSpec with Matchers {
  // Exercise 11.3: sequence & traverse
  behavior of "sequence"

  it should "transform a list of monad into a monad of list" in {
    val in = List(Some(1), Some(2), Some(3))
    optionMonad.sequence(in) should be (Some(List(1, 2, 3)))
  }

  it should "transform a list of options with a single None into a None" in {
    val in = List(Some(1), None, Some(3))
    optionMonad.sequence(in) should be (None)
  }


  behavior of "traverse"

  it should "traverse a list into an option" in {
    val in = List(1, 2, 3)
    val f = (i: Int) => Some(i)
    optionMonad.traverse(in)(f) should be (Some(List(1, 2, 3)))
  }

  it should "traverse a list into a none" in {
    val in = List(1, 2, 3)
    val f = (i: Int) => if (i % 2 == 0) None else Some(i)
    optionMonad.traverse(in)(f) should be (None)
  }


  // Exercise 11.4: replicateM
  behavior of "replicateM"

  it should "replicate a Some" in {
    optionMonad.replicateM(3, Some(42)) should be (Some(List(42, 42, 42)))
  }

  it should "not replicate a None" in {
    optionMonad.replicateM(3, None) should be (None)
  }


  // Exercise 11.6: filterM
  behavior of "filterM"

  it should "filter to Some" in {
    val in = List(1, 2, 3, 4)
    val f = (i: Int) => if (i % 2 == 0) Some(true) else Some(false)
    optionMonad.filterM(in)(f) should be (Some(List(2, 4)))
  }

  it should "filter to None" in {
    val in = List(1, 2, 3, 4)
    val f = (i: Int) => if (i % 2 == 0) Some(true) else None
    optionMonad.filterM(in)(f) should be (None)
  }


  // Exercise 11.8: flatMap in terms of compose
  behavior of "_flatMap"

  it should "return None for None" in {
    optionMonad._flatMap[Int, Int](None)(i => Some(i + 1)) should be (None)
  }

  it should "return Some for Some" in {
    optionMonad._flatMap(Some(1))(i => Some(i + 1)) should be (Some(2))
  }


  // Exercise 11.12: join
  behavior of "join"

  it should "join over None" in {
    optionMonad.join(None) should be (None)
    optionMonad.join(Some(None)) should be (None)
  }

  it should "join over Some" in {
    optionMonad.join(Some(Some(42))) should be (Some(42))
  }


  // Exercise 11.13: flatMap in terms of join
  behavior of "__flatMap"

  it should "return None for None" in {
    optionMonad.__flatMap[Int, Int](None)(i => Some(i + 1)) should be (None)
  }

  it should "return Some for Some" in {
    optionMonad.__flatMap(Some(1))(i => Some(i + 1)) should be (Some(2))
  }
}
