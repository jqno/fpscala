package nl.jqno.fpscala.ch6_state

import org.scalatest.{FlatSpec, Matchers}
import State._

class StateTest extends FlatSpec with Matchers {

  val rng = SimpleRNG(1337)
  def unit[A](n: A): State[RNG, A] = State(RNGFunctions.unit(n))


  behavior of "map"

  it should "be like map" in {
    unit(42).map(_ + 1).run(rng)._1 should be (43)
  }


  behavior of "map2"

  it should "behave like intDouble" in {
    val sa = unit(2)
    val sb = unit(3)
    val mapped = map2(sa, sb)(_ + _)
    mapped.run(rng)._1 should be (5)
  }


  behavior of "flatMap"

  it should "be like flatMap" in {
    unit(42).flatMap(n => State(RNGFunctions.unit(n + 1))).run(rng)._1 should be (43)
  }


  behavior of "sequence"

  it should "be like sequence" in {
    val randomInts = List(unit(1), unit(2), unit(3))
    sequence(randomInts).run(rng)._1 should be (List(1, 2, 3))
  }
}
