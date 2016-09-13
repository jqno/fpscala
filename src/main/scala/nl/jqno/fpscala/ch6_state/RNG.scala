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


  // Taken from the book
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5: more elegant double
  def double2: Rand[Double] =
    map(int)(i => (i.toDouble / Int.MaxValue / 2) + 0.5D)


  // 6.6: map2
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }


  // Taken from the book
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))


  // 6.7: sequence
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((cur, acc) => map2(cur, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))


  // 6.8: flatMap
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = r(rng)
    f(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }


  // 6.9: map, map2 in terms of flatMap
  def map_2[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}
