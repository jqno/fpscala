package nl.jqno.fpscala.ch10_monoids

import scala.language.higherKinds
import nl.jqno.fpscala.ch3_functional_data_structures.{Tree, Leaf, Branch}
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


  // Exercise 10.9: isSorted
  val sortedMonoid = new Monoid[(Int, Int, Boolean)] {
    def op(a1: (Int, Int, Boolean), a2: (Int, Int, Boolean)) = {
      val (min1, max1, b1) = a1
      val (min2, max2, b2) = a2
      (min1 min min2, max1 max max2, b1 && b2 && min1 <= min2 && max1 <= max2)
    }
    val zero = (Integer.MIN_VALUE, Integer.MAX_VALUE, false)
  }
  def isSorted(as: IndexedSeq[Int]): Boolean =
    foldMap(as, sortedMonoid)(i => (i, i, true))._3


  // Exercise: 10.10: wordcount monoid
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC
  val wcMonoid = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(s1), Stub(s2)) =>
        Stub(s1 + s2)
      case (Stub(s1), Part(ls2, w2, rs2)) =>
        Part(s1 + ls2, w2, rs2)
      case (Part(ls1, w1, rs1), Stub(s2)) =>
        Part(ls1, w1, rs1 + s2)
      case (Part(ls1, w1, rs1), Part(ls2, w2, rs2)) =>
        Part(ls1, w1 + w2 + (if ((rs1 + ls2).isEmpty) 0 else 1), rs2)
    }
    val zero = Stub("")
  }

  // Exercise 10.11: wordcount
  val wcf = (c: Char) => if (" ,.()" contains c) Part("", 0, "") else Stub(c.toString)
  val unstub = (s: String) => s.length min 1
  def wordcount(sentence: String): Int =
    foldMap(sentence, wcMonoid)(wcf) match {
      case Stub(s) => unstub(s)
      case Part(l, words, r) => unstub(l) + words + unstub(r)
    }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}


// Exercise 10.12: foldables for List, IndexedSeq, Stream
object FoldableList extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object FoldableIndexedSeq extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object FoldableStream extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}


// Exercise 10.13: foldable Tree
object FoldableTree extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(x) => f(x, z)
    case Branch(a, b) => foldRight(a)(foldRight(b)(z)(f))(f)
  }
  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(x) => f(z, x)
    case Branch(a, b) => foldLeft(b)(foldLeft(a)(z)(f))(f)
  }
  def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(x) => f(x)
    case Branch(a, b) => mb.op(foldMap(a)(f)(mb), foldMap(b)(f)(mb))
  }
}


// Exercise 10.14: foldable Option
object FoldableOption extends Foldable[Option] {
  def foldRight[A, B](opt: Option[A])(z: B)(f: (A, B) => B): B = opt.foldRight(z)(f)
  def foldLeft[A, B](opt: Option[A])(z: B)(f: (B, A) => B): B = opt.foldLeft(z)(f)
  def foldMap[A, B](opt: Option[A])(f: A => B)(mb: Monoid[B]): B = opt.map(f).getOrElse(mb.zero)
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
  def intBoolTuples = for {
    i <- ints
    b <- Gen.boolean
  } yield (i, i, b)
  def stubs: Gen[WC] = strings.map(Stub.apply)
  def parts: Gen[WC] = for {
    lStub <- strings
    words <- Gen.choose(0, 10)
    rStub <- strings
  } yield Part(lStub, words, rStub)
  def toilets = Gen.weighted[WC]((stubs, 0.5), (parts, 0.5))
  
  run(monoidLaws(stringMonoid, strings))
  run(monoidLaws[List[Int]](listMonoid, ints.listOfN(positiveInts)))
  run(monoidLaws(intAddition, ints))
  run(monoidLaws(intMultiplication, ints))
  run(monoidLaws(booleanOr, Gen.boolean))
  run(monoidLaws(booleanAnd, Gen.boolean))
  run(monoidLaws[Option[Int]](optionMonoid, optionsOf(ints)))
  run(monoidLaws[Option[Int]](dualOptionMonoid, optionsOf(ints)))
  // testing the endomonoids would be kind of annoying with the current test framework, so I'm skipping them.
  run(monoidLaws(sortedMonoid, intBoolTuples))
  run(monoidLaws(wcMonoid, toilets))
}

