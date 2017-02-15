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


  // Exercise 8.9: && and ||
  behavior of "&&"

  it should "behave like logical and" in {
    val p1 = Prop.forAll(Gen.unit(1))(_ == 1)
    val p2 = Prop.forAll(Gen.unit(2))(_ == 1)
    run(p1 && p2) should be (Falsified("2", 10))
  }

  behavior of "||"

  it should "behave like logical or" in {
    val p1 = Prop.forAll(Gen.unit(1))(_ == 1)
    val p2 = Prop.forAll(Gen.unit(2))(_ == 2)
    run(p1 || p2) should be (Passed)
  }


  private def run(p: Prop): Result =
    p.run(0, 10, rng)
}
