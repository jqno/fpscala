package nl.jqno.fpscala.ch8_testing

import nl.jqno.fpscala.ch5_strictness_and_laziness.Stream
import nl.jqno.fpscala.ch6_state._
import nl.jqno.fpscala.ch7_parallelism._
import nl.jqno.fpscala.ch7_parallelism.Par.Par
import Gen._
import Prop._
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
}

case class Prop(run: (TestCases, RNG) => Result) {

  // Exercise 8.9: && and ||
  def && (p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng) match {
        case Passed => Passed
        case Falsified(f, s) => Falsified(f, s + n)
      }
      case f: Falsified => f
    }
  }

  def || (p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => Passed
      case f: Falsified => p.run(n, rng) match {
        case Passed => Passed
        case Falsified(f2, s2) => Falsified(f2, f.successes + s2)
      }
    }
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        }
        catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

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

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}" +
    s"stack trace: ${e.getStackTrace.mkString("\n")}"
}

case class SGen[A](forSize: Int => Gen[A]) {

  // Exercise 8.11: delegations
  def map[B](f: A => B): SGen[B] =
    SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))

  def listOfN(size: SGen[Int]): SGen[List[A]] =
    SGen(i => forSize(i).listOfN(size.forSize(i)))
}

