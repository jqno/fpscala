package nl.jqno.fpscala.ch8_testing

import org.scalatest.{FlatSpec, Matchers}
import nl.jqno.fpscala.ch6_state.SimpleRNG
import Prop._

class SGenTest extends FlatSpec with Matchers {
  val rng = SimpleRNG(1337)

  // Exercise 8.10: convert Gen to SGen
  behavior of "unsized"

  it should "convert a Gen to an SGen" in {
    val g = Gen.unit(1)
    g.unsized.forSize(0) should be (g)
  }
}
