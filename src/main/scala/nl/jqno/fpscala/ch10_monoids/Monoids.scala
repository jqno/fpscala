package nl.jqno.fpscala.ch10_monoids

import nl.jqno.fpscala.ch7_parallelism.Par
import nl.jqno.fpscala.ch7_parallelism.Par._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = List.empty[A]
  }


  // Exercise 10.1: monoids for Int and Boolean
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }


  // Exercise 10.2: a monoid for Options
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }
  def dualOptionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a2 orElse a1
    val zero = None
  }


  // Exercise 10.3: endoMonoid
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    val zero = (a: A) => a
  }
  def dualEndoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a2 andThen a1
    val zero = (a: A) => a
  }


  // Exercise 10.5: foldMap
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))


  // Exercise 10.6: foldRight in terms of foldMap
  // Oops: I peeked
  def foldRight1[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)


  // Exercise 10.7: balanced foldMap
  def foldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = as.size match {
    case 0 => m.zero
    case 1 => f(as(0))
    case _ =>
      val mid = as.length / 2
      val (bs, cs) = as.splitAt(mid)
      m.op(foldMap(bs, m)(f), foldMap(cs, m)(f))
  }


  // Exercise 10.8: parFoldMap
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]) = Par.map2noTimeout(a1, a2)(m.op)
    val zero = Par.unit(m.zero)
  }
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = as.size match {
    case 0 => par(m).zero
    case 1 => Par.unit(f(as(0)))
    case _ =>
      val mid = as.length / 2
      val (bs, cs) = as.splitAt(mid)
      par(m).op(parFoldMap(bs, m)(f), parFoldMap(cs, m)(f))
  }
}

object MonoidLaws extends App {
  import nl.jqno.fpscala.ch8_testing._
  import Prop._
  import Monoids._

  // Exercise 10.4: property-based testing
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop ={
    val identityLaw = forAll(gen) { a =>
      m.op(m.zero, a) == m.op(a, m.zero)
    }
    val associativityLaw = forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)) { case (x, y, z) =>
      m.op(m.op(x, y), z) == m.op(x, m.op(y, z))
    }
    identityLaw && associativityLaw
  }


  def ints = Gen.choose(-10, 10)
  def positiveInts = Gen.choose(0, 20)
  def strings = positiveInts.flatMap(n => Gen.listOfN(n, Gen.choose(0,127)).map(_.map(_.toChar).mkString))
  def optionsOf[A](g: Gen[A]) = Gen.weighted[Option[A]](
    (Gen.unit(None), 0.2),
    (g.map(a => Some(a)), 0.8))
  
  run(monoidLaws(stringMonoid, strings))
  run(monoidLaws[List[Int]](listMonoid, ints.listOfN(positiveInts)))
  run(monoidLaws(intAddition, ints))
  run(monoidLaws(intMultiplication, ints))
  run(monoidLaws(booleanOr, Gen.boolean))
  run(monoidLaws(booleanAnd, Gen.boolean))
  run(monoidLaws[Option[Int]](optionMonoid, optionsOf(ints)))
  run(monoidLaws[Option[Int]](dualOptionMonoid, optionsOf(ints)))
  // testing the endomonoids would be kind of annoying with the current test framework, so I'm skipping them.
}

