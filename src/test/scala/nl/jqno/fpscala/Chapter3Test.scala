package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}

class Chapter3Test extends FlatSpec with Matchers {
  val someList = List(1, 2, 3, 4, 5, 6)

  behavior of "pattern matching"

  it should "be 3" in {
    Chapter3.x should be (3)
  }


  behavior of "tail"

  it should "take the tail of a list" in {
    someList.tail should be (List(2, 3, 4, 5, 6))
  }

  it should "throw when tailing a Nil" in {
    intercept[IllegalStateException] {
      Nil.tail
    }
  }
}
