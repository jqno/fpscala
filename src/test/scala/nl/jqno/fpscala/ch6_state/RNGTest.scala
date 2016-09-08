package nl.jqno.fpscala.ch6_state

import org.scalatest.{FlatSpec, Matchers}

class RNGTest extends FlatSpec with Matchers {

  behavior of "nextInt"

  it should "behave like in the book" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, _) = rng2.nextInt
    n1 should be (16159453)
    n2 should be (-1281479697)
  }
}
