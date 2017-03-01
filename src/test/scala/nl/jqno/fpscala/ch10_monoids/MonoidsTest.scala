package nl.jqno.fpscala.ch10_monoids

import org.scalatest.{FlatSpec, Matchers}
import Monoids._

class MonoidsTest extends FlatSpec with Matchers {
  // Exercise 10.5: foldMap
  behavior of "foldMap"

  it should "map the values of a list and fold over the results" in {
    val as = List("1", "2", "3", "4")
    foldMap(as, intAddition)(_.toInt) should be (10)
  }


  // Exercise 10.6: foldRight in terms of foldMap
  behavior of "foldRight1"

  it should "fold over a list" in {
    val as = List("1", "2", "3", "4")
    foldRight1(as)(0)((a, b) => a.toInt + b) should be (10)
  }
}

