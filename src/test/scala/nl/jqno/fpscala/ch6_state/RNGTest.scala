package nl.jqno.fpscala.ch6_state

import org.scalatest.{FlatSpec, Matchers}
import RNGFunctions._

class RNGTest extends FlatSpec with Matchers {

  val epsilon = 0.00001D


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
    double(rng)._1 should be (1.0D +- epsilon)
  }

  it should "return 0 if RNG gives Int.MinValue" in {
    val rng = SeedReturningRNG(Int.MinValue)
    double(rng)._1 should be (0.0D +- epsilon)
  }


  behavior of "intDouble"

  it should "return an Int and a Double, which are derived from a different seed" in {
    val rng = SeedReturningRNG(Int.MinValue)
    val result = intDouble(rng)._1
    result._1 should be (Int.MinValue)
    result._2 should not be (0.0D +- epsilon)
  }

  it should "return a fresh RNG" in {
    val rng = SimpleRNG(42)
    val (result, newRng) = intDouble(rng)
    newRng.nextInt._1 should not be result._1
    double(newRng)._1 should not be (result._2 +- epsilon)
  }


  behavior of "doubleInt"

  it should "return a Double and an Int, which are derived from a different seed" in {
    val rng = SeedReturningRNG(Int.MaxValue)
    val result = doubleInt(rng)._1
    result._1 should be (1.0D +- epsilon)
    result._2 should not be Int.MaxValue
  }

  it should "return a fresh RNG" in {
    val rng = SimpleRNG(42)
    val (result, newRng) = doubleInt(rng)
    double(newRng)._1 should not be (result._1 +- epsilon)
    newRng.nextInt._1 should not be result._2
  }


  behavior of "double3"

  it should "return 3 different Doubles" in {
    val rng = SimpleRNG(0)
    val result = double3(rng)._1
    result._1 should not be result._2
    result._1 should not be result._3
    result._2 should not be result._3
  }

  it should "return a fresh RNG" in {
    val rng = SimpleRNG(42)
    val (result, newRng) = double3(rng)
    double(newRng)._1 should not be (result._1 +- epsilon)
    double(newRng)._1 should not be (result._2 +- epsilon)
    double(newRng)._1 should not be (result._3 +- epsilon)
  }


  behavior of "ints"

  it should "return a list of random, non-equal integers" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints(4)(rng)
    result.combinations(2).foreach(ns => ns(0) should not be ns(1))
  }

  it should "return a list of the correct length" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints(3)(rng)
    result.size should be (3)
  }

  it should "return a fresh RNG" in {
    val rng = SimpleRNG(42)
    val (result, newRng) = ints(3)(rng)
    result.foreach(n => newRng.nextInt._1 should not be n)
  }

  it should "return an empty List when count == 0" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints(0)(rng)
    result should have size 0
  }

  it should "return an empty list when count < 0" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints(-1)(rng)
    result should have size 0
  }


  behavior of "double2"

  it should "return 1 if RNG gives Int.MaxValue" in {
    val rng = SeedReturningRNG(Int.MaxValue)
    double2(rng)._1 should be (1.0D +- epsilon)
  }

  it should "return 0 if RNG gives Int.MinValue" in {
    val rng = SeedReturningRNG(Int.MinValue)
    double2(rng)._1 should be (0.0D +- epsilon)
  }


  behavior of "map2"

  it should "behave like intDouble" in {
    val intDouble2 = map2(int, double)((_, _))
    val rng = SeedReturningRNG(Int.MinValue)
    val result = intDouble2(rng)._1
    result._1 should be (Int.MinValue)
    result._2 should not be (0.0D +- epsilon)
  }


  behavior of "ints2, via sequence"

  it should "return a list of random, non-equal integers" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints2(4)(rng)
    result.combinations(2).foreach(ns => ns(0) should not be ns(1))
  }

  it should "return a list of the correct length" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints2(3)(rng)
    result.size should be (3)
  }

  it should "return a singleton List when count == 1" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints2(1)(rng)
    result should have size 1
  }

  it should "return an empty List when count == 0" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints2(0)(rng)
    result should have size 0
  }

  it should "return an empty list when count < 0" in {
    val rng = SimpleRNG(42)
    val (result, _) = ints2(-1)(rng)
    result should have size 0
  }


  behavior of "nonNegativeLessThan"

  it should "always return 0 if n == 1" in {
    val rng = SeedReturningRNG(42)
    nonNegativeLessThan(1)(rng)._1 should be (0)
  }

  it should "retry with a second RNG if the complicated condition is met" in {
    val firstAttemptRng = SeedReturningRNG(Int.MaxValue)
    val (unexpected, secondAttemptRng) = map(nonNegativeInt)(_ % 5)(firstAttemptRng)
    val (expected, _) = map(nonNegativeInt)(_ % 5)(secondAttemptRng)

    val (actual, _) = nonNegativeLessThan(5)(firstAttemptRng)

    actual should not be unexpected
    actual should be (expected)
  }


  behavior of "map and map2 in terms of flatMap"

  it should "be like map" in {
    val rng = SimpleRNG(0)
    val r = unit(42)
    val actual = map_2(r)(_ + 1)(rng)._1
    actual should be (43)
  }

  it should "behave like intDouble" in {
    val intDouble2 = map2_2(int, double)((_, _))
    val rng = SeedReturningRNG(Int.MinValue)
    val result = intDouble2(rng)._1
    result._1 should be (Int.MinValue)
    result._2 should not be (0.0D +- epsilon)
  }


  case class SeedReturningRNG(seed: Int) extends RNG {
    override def nextInt: (Int, RNG) = (seed, SimpleRNG(seed))
  }
}
