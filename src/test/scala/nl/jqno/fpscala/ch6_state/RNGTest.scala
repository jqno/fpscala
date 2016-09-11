package nl.jqno.fpscala.ch6_state

import org.scalatest.{FlatSpec, Matchers}
import RNGFunctions._

class RNGTest extends FlatSpec with Matchers {

  behavior of "nextInt"

  it should "behave like in the book" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, _) = rng2.nextInt
    n1 should be (16159453)
    n2 should be (-1281479697)
  }


  behavior of "nonNegativeInt"

  it should "return a positive int if RNG gives a positive int" in {
    val rng = SeedReturningRNG(1)
    nonNegativeInt(rng)._1 should be (1)
  }

  it should "return a positive int if RNG gives a negative int" in {
    val rng = SeedReturningRNG(-1)
    nonNegativeInt(rng)._1 should be (1)
  }

  it should "return 0 if RNG gives 0" in {
    val rng = SeedReturningRNG(0)
    nonNegativeInt(rng)._1 should be (0)
  }

  it should "return the next value if RNG gives Int.MinValue" in {
    val expected = SimpleRNG(Int.MinValue)
    val actual = SeedReturningRNG(Int.MinValue)
    nonNegativeInt(actual)._1 should be (nonNegativeInt(expected)._1)
  }


  behavior of "double"

  it should "return 1 if RNG gives Int.MaxValue" in {
    val rng = SeedReturningRNG(Int.MaxValue)
    double(rng)._1 should be (1.0D +- 0.00001D)
  }

  it should "return 0 if RNG gives Int.MinValue" in {
    val rng = SeedReturningRNG(Int.MinValue)
    double(rng)._1 should be (0.0D +- 0.00001D)
  }


  case class SeedReturningRNG(seed: Int) extends RNG {
    override def nextInt: (Int, RNG) = (seed, SimpleRNG(seed))
  }
}
