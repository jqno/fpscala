package nl.jqno.fpscala.ch8_testing

import nl.jqno.fpscala.ch5_strictness_and_laziness.Stream
import nl.jqno.fpscala.ch6_state._
import nl.jqno.fpscala.ch7_parallelism._
import nl.jqno.fpscala.ch7_parallelism.Par.Par
import Gen._
import Prop._
import StupidThreadPools._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// Exercise 8.1: properties for sum
// * The sum of a list with all elements of the same value, should be that value times the lenght of the list
// * The sum of a list with 1 element should be that element
// * The sum of an empty list shoud be zero
// * The sum of a list with only positive elements should be positive
// * The sum of a list with only positive elements should be greater than the sum of a shorter list with only positive elements

// Exercise 8.2: properties of max
// * The max of a reversed list should be the same as the max of the original, non-reversed list
// * The max of a list with 1 element should be that element
// * The max of a list with only positive elements should be positive
// * The max of a list should be greater than or equal to the max of the same list with the original max taken out

// Exercise 8.3: &&
//
// def && (p: Prop): Prop = {
//   val self = this
//   new Prop {
//     override def check = self.check && p.check
//   }
// }

object Gen {
  // Exercise 8.4: Gen.choose
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNGFunctions.nonNegativeInt).map(n => start + n % (stopExclusive - start)))


  // Exercise 8.5: unit, boolean, listOfN
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNGFunctions.nonNegativeLessThan(2)).map(i => if (i == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))


  // Exercise 8.7: union
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)


  // Exercise 8.8: weighted
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val total = g1._2 + g2._2
    Gen(State(RNGFunctions.double)).flatMap { d =>
      if (d * total <= g1._2) g1._1 else g2._1
    }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  // Exercise 8.6: flatMap and listOfN
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))


  // Exercise 8.10: convert Gen to SGen
  def unsized: SGen[A] = SGen(_ => this)



  def map2[B, C](gen: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(State.map2(sample, gen.sample)(f))

  def ** [B](gen: Gen[B]): Gen[(A, B)] =
    (this map2 gen)((_, _))
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // Exercise 8.9: && and ||
  def && (p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case f: Falsified => f
      case a: Result => p.run(max, n, rng) match {
        case Passed => Passed
        case Proven => a
        case Falsified(f, s) => Falsified(f, s + n)
      }
    }
  }

  def || (p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case f: Falsified => p.run(max, n, rng) match {
        case Falsified(f2, s2) => Falsified(f2, f.successes + s2)
        case Passed => Passed
        case Proven => Proven
      }
      case a => a
    }
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        }
        catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](gen: SGen[A])(f: A => Boolean): Prop =
    forAll(gen(_))(f)

  def forAll[A](gen: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(gen(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proven else Falsified("()", 0)
  }

  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2noTimeout(p1, p2)(_ == _)

  private val S = weighted(
    choose(1, 4).map(n => stupid(Executors.newFixedThreadPool(n))) -> 0.75,
    unit(stupid(Executors.newCachedThreadPool)) -> 0.25)

  def forAllPar[A](gen: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** gen) { case s ** a => f(a)(s).get }

  def checkPar(p: => Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proven =>
        println(s"+ OK, proven property.")
    }

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proven extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}" +
    s"stack trace: ${e.getStackTrace.mkString("\n")}"
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  // Exercise 8.11: delegations
  def map[B](f: A => B): SGen[B] =
    SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))

  def listOfN(size: SGen[Int]): SGen[List[A]] =
    SGen(i => forSize(i).listOfN(size.forSize(i)))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => Gen.listOfN(i, g))


  // Exercise 8.13: listOf1
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => Gen.listOfN(i max 1, g))
}

object StupidThreadPools {
  var pools = Set.empty[ExecutorService]

  def stupid(p: ExecutorService): ExecutorService = {
    pools += p
    p
  }

  def shutdown(): Unit =
    pools.foreach(_.shutdown())
}
