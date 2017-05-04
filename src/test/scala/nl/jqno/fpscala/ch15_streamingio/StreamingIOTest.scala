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


  // Exercise 15.3: mean
  behavior of "Process.mean"

  it should "keep a running average" in {
    mean(someStream.map(_.toDouble)).toList should be (Stream(1, 1.5, 2, 2.5))
  }


  // Exercise 15.4: loop
  behavior of "loop"

  it should "keep a running sum" in {
    sum2(someStream.map(_.toDouble)).toList should be (List(1, 3, 6, 10))
  }

  it should "keep a running count" in {
    count3(someStream).toList should be (Stream(1, 2, 3, 4))
  }


  // Exercise 15.5: |>
  behavior of "|>"

  it should "compose two instances of Process" in {
    (drop(2) |> sum)(someStream.map(_.toDouble)).toList should be (List(3, 7))
  }


  // Exercise 15.6: zipWithIndex and 15.7: mean in terms of sum & count
  //
  // I'm skipping 15.6 because I couldn't figure out how to make it work
  // (and it's weird because it's the only operation not defined on the
  // companion object for some reason). There's no hint and the answerKey
  // relies on zip, a thing that belongs to the next exercise like it's a
  // normal thing to just do exercises in a random order or something.
  //
  // Then the next exercise has no hint either, is marked as hard, and
  // nowhere suggests that you have to make a zip function either, and now
  // I'm just kind of annoyed with both of them. 😅
}

