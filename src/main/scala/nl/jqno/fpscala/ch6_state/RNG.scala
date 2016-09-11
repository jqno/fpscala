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


  // 6.2: double
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = rng.nextInt
    val result = (n.toDouble / Int.MaxValue / 2) + 0.5D
    (result, nextRng)
  }


  // 6.3: intDouble, doubleInt, double3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }


  // 6.4: ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) {
      (Nil, rng)
    }
    else {
      val (n, rng1) = rng.nextInt
      val (ns, rng2) = ints(count - 1)(rng1)
      (n :: ns, rng2)
    }
}
