package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}

class Chapter3Test extends FlatSpec with Matchers {
  behavior of "pattern matching"

  it should "be 3" in {
    Chapter3.x should be (3)
  }
}
