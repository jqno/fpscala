package nl.jqno.fpscala.ch6_state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNGFunctions {

  // 6.1: nonNegativeInt
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if (n == Int.MinValue)
      nonNegativeInt(nextRng)
    else if (n < 0)
      (-n, nextRng)
    else
      (n, nextRng)
  }
}
