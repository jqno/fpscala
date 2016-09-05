package nl.jqno.fpscala.ch5_strictness_and_laziness

import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}
import Stream._

import scala.collection.mutable

class StreamTest extends FlatSpec with Matchers with OneInstancePerTest {
  val stream = Stream(1, 2, 3, 4)

  val stack = mutable.MutableList.empty[Int]
  def stackingStream: Stream[Int] =
    cons({ stack += 1; 1 },
    cons({ stack += 2; 2 },
    cons({ stack += 3; 3 },
    cons({ stack += 4; 4 },
    Empty))))

  val even = (i: Int) => i % 2 == 0


  behavior of "stack"

  it should "initially be empty" in {
    stack.isEmpty should be (true)
  }

  it should "not be empty when we realize items of the stackingStream" in {
    stackingStream.toList
    stack should be (List(1, 2, 3, 4))
  }


  behavior of "toList"

  it should "convert a Stream into a List with the same values" in {
    val actual = stream.toList
    classOf[List[_]].isAssignableFrom(actual.getClass) should be (true)
    actual should be (List(1, 2, 3, 4))
  }

  behavior of "take"

  it should "stop when empty" in {
    Empty.take(2).toList should be (Nil)
    stream.take(5).toList should be (stream.toList)
  }

  it should "take the first elements of a stream" in {
    stream.take(2).toList should be (List(1, 2))
  }

  it should "be lazy" in {
    val actual = stackingStream.take(2)
    stack.isEmpty should be (true)
    actual.toList should be (List(1, 2))
    stack should be (List(1, 2))
  }


  behavior of "drop"

  it should "stop when empty" in {
    Empty.drop(2).toList should be (Nil)
    stream.drop(5).toList should be (Nil)
  }

  it should "drop the first elements of a stream" in {
    stream.drop(2).toList should be (List(3, 4))
  }

  it should "be lazy" in {
    stackingStream.drop(2)
    stack.isEmpty should be (true)
  }


  behavior of "takeWhile"

  it should "stop when empty" in {
    Empty.takeWhile(_ => true).toList should be (Nil)
  }

  it should "take elements until one doesn't satisfy the predicate" in {
    Stream(2, 4, 6, 7, 8, 9, 10).takeWhile(even).toList should be (List(2, 4, 6))
  }

  it should "be lazy" in {
    val actual = stackingStream.takeWhile(_ <= 2)

    // It evaluates the predicate for the first element, then stops because it's lazy.
    stack should be (List(1))

    actual.toList should be (List(1, 2))

    // After realizing the actual stream, it has evaluated its elements
    // plus one more, because it needs to evaluate the predicate on that.
    stack should be (List(1, 2, 3))
  }
}
