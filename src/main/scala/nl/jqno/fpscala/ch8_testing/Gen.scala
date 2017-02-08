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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

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
}

case class Gen[A](sample: State[RNG, A]) {
  // Exercise 8.6: flatMap and listOfN
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    ???

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
}

trait SGen[+A] {

}
