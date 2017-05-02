package nl.jqno.fpscala.ch15_streamingio

import org.scalatest.{FlatSpec, Matchers}
import SimpleStreamTransducers.Process._

class StreamingIOTest extends FlatSpec with Matchers {

  val someStream = Stream(1, 2, 3, 4)


  behavior of "Process"

  it should "behave as described in the book" in { 
    sum(someStream.map(_.toDouble)).toList should be (List(1, 3, 6, 10))
  }


  // Exercise 15.1: take & drop
  behavior of "Process.take"

  it should "take the first n elements from a stream, and then stop" in {
    take(2)(someStream).toList should be (List(1, 2))
  }

  behavior of "Process.drop"

  it should "drop the first n elements from a stram, and then continue" in {
    drop(2)(someStream).toList should be (List(3, 4))
  }

  behavior of "Process.takeWhile"

  it should "take elements while the predicate is true, and then stop" in {
    takeWhile[Int](_ < 3)(someStream).toList should be (List(1, 2))
  }

  behavior of "Process.dropWhile"

  it should "drop elements while the predicate is true, and then continue" in {
    dropWhile[Int](_ < 3)(someStream).toList should be (List(3, 4))
  }


  // Exercise 15.2: count
  behavior of "Process.count"

  it should "keep count of the number of elements seen so far" in {
    count(someStream).toList should be (Stream(1, 2, 3, 4))
  }
}

