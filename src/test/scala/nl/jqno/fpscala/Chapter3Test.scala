package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}
import Chapter3._

class Chapter3Test extends FlatSpec with Matchers {
  val someList = List(1, 2, 3, 4, 5, 6)

  behavior of "pattern matching"

  it should "be 3" in {
    Chapter3.x should be (3)
  }


  behavior of "tail"

  it should "take the tail of a list" in {
    tail(someList) should be (List(2, 3, 4, 5, 6))
  }

  it should "throw when tailing a Nil" in {
    intercept[IllegalStateException] {
      tail(Nil)
    }
  }


  behavior of "setHead"

  it should "replace the head of a list" in {
    setHead(42, someList) should be (List(42, 2, 3, 4, 5, 6))
  }

  it should "throw when setHeading a Nil" in {
    intercept[IllegalStateException] {
      setHead(0, Nil)
    }
  }


  behavior of "drop"

  it should "drop the first n elements from a list" in {
    drop(someList, 2) should be (List(3, 4, 5, 6))
    drop(someList, 0) should be (someList)
  }
}
