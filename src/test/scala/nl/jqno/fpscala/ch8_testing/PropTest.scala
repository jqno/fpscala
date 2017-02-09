package nl.jqno.fpscala.ch8_testing

import org.scalatest.{FlatSpec, Matchers}
import nl.jqno.fpscala.ch6_state.SimpleRNG
import Prop._

class PropTest extends FlatSpec with Matchers {
  val rng = SimpleRNG(1337)

  behavior of "Prop.forAll"

  it should "run a bunch of tests" in {
    val g = Gen.choose(0, 10)
    val p = Prop.forAll(g)(n => n >= 0 && n < 10)
    run(p) should be (Passed)
  }

  it should "fail if the condition is false and there's enough TestCases" in {
    val g = Gen.choose(0, 10)
    val p = Prop.forAll(g)(n => n >= 5 && n < 10)
    run(p) should be (Falsified(failure = "0", successes = 1))
  }


  private def run(p: Prop): Result =
    p.run(10, rng)
}
