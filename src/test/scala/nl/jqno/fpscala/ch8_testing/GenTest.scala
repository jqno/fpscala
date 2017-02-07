package nl.jqno.fpscala.ch8_testing

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures
import nl.jqno.fpscala.ch6_state.SimpleRNG

class GenTest extends FlatSpec with Matchers {
  val rng = SimpleRNG(1337)

  // Exercise 8.4: Gen.choose
  behavior of "Gen.choose"

  it should "generate integers in the range start to stopExclusive" in {
    val a = Gen.choose(0, 1)
    force(a) should be (0)

    val b = Gen.choose(1, 2)
    force(b) should be (1)
    
    val c = Gen.choose(0, 100)
    force(c) should be (79)
  }


  // Exercise 8.5: unit, boolean, listOfN
  behavior of "unit"

  it should "generate the given value" in {
    val a = Gen.unit("hello world")
    force(a) should be ("hello world")
  }

  behavior of "boolean"

  it should "generate a boolean" in {
    val a = Gen.boolean
    force(a) should be (false)
  }

  private def force[A](gen: Gen[A]): A =
    gen.sample.run(rng)._1
}
