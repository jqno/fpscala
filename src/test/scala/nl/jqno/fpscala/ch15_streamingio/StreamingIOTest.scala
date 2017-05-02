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
}

