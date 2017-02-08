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
  behavior of "Gen.unit"

  it should "generate the given value" in {
    val a = Gen.unit("hello world")
    force(a) should be ("hello world")
  }

  behavior of "Gen.boolean"

  it should "generate a boolean" in {
    val a = Gen.boolean
    force(a) should be (false)
  }

  behavior of "Gen.listOfN"

  it should "generate a list of things" in {
    val a = Gen.listOfN(3, Gen.boolean)
    force(a) should be (List(false, true, false))

    val b = Gen.listOfN(0, Gen.boolean)
    force(b) should be (Nil)
  }


  // Exercise 8.6: flatMap and listOfN
  behavior of "listOfN"

  it should "generate a list of unspecified length" in {
    val a = Gen.unit(0).listOfN(Gen.choose(0, 10))
    force(a) should be (List(0, 0, 0))
  }

  private def force[A](gen: Gen[A]): A =
    gen.sample.run(rng)._1
}
