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
}
