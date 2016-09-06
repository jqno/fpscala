package nl.jqno.fpscala.ch5_strictness_and_laziness

import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}
import Stream._

import scala.collection.mutable

class StreamTest extends FlatSpec with Matchers with OneInstancePerTest {
  val stream = Stream(1, 2, 3, 4)
  val even = (i: Int) => i % 2 == 0

  val stack = mutable.MutableList.empty[Int]
  def stackingStream: Stream[Int] =
    cons(addToStack(1),
    cons(addToStack(2),
    cons(addToStack(3),
    cons(addToStack(4),
    Empty))))
  val addToStack = (i: Int) => { stack += i; i }
  val fullStack = List(1, 2, 3, 4)
  val emptyStack = Nil


  behavior of "stack"

  it should "initially be empty" in {
    stack should be (emptyStack)
  }

  it should "not be empty when we realize items of the stackingStream" in {
    stackingStream.toList
    stack should be (fullStack)
  }


  behavior of "toList"

  it should "convert a Stream into a List with the same values" in {
    val actual = stream.toList
    classOf[List[_]].isAssignableFrom(actual.getClass) should be (true)
    actual should be (fullStack)
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
    stack should be (emptyStack)
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
    stack should be (emptyStack)
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


  behavior of "forAll"

  it should "return correct results" in {
    stream.forAll(even) should be (false)
    stream.forAll(_ < 6) should be (true)
  }

  it should "short-circuit" in {
    val actual = stackingStream.forAll(_ < 2)
    stack should be (List(1, 2))
    actual should be (false)
  }


  behavior of "takeWhile using foldRight"

  it should "stop when empty" in {
    Empty.takeWhile2(_ => true).toList should be (Nil)
  }

  it should "take elements until one doesn't satisfy the predicate" in {
    Stream(2, 4, 6, 7, 8, 9, 10).takeWhile2(even).toList should be (List(2, 4, 6))
  }

  it should "be lazy" in {
    val actual = stackingStream.takeWhile2(_ <= 2)
    stack should be (List(1))
    actual.toList should be (List(1, 2))
    stack should be (List(1, 2, 3))
  }


  behavior of "headOption"

  it should "return None on an empty Stream" in {
    Empty.headOption should be (None)
  }

  it should "return the head in a Some on a non-empty Stream" in {
    stream.headOption should be (Some(1))
  }

  it should "be lazy" in {
    stackingStream.headOption
    stack should be (List(1))
  }


  behavior of "map"

  it should "return empty when the Stream is empty" in {
    Empty.map(identity).toList should be (Nil)
  }

  it should "map over all the values" in {
    stream.map(_.toString).toList should be (List("1", "2", "3", "4"))
  }

  it should "be lazy" in {
    val actual = stackingStream.map(_.toString)
    stack should be (List(1))
    actual.toList
    stack should be (fullStack)
  }


  behavior of "filter"

  it should "return empty when the Stream is empty" in {
    Empty.filter(_ => true).toList should be (Nil)
  }

  it should "filter out all elements where the predicate is false" in {
    stream.filter(even).toList should be (List(2, 4))
  }

  it should "be lazy" in {
    val actual = stackingStream.filter(even)
    stack should be (List(1, 2)) // why? <-- because it continues until it encounters a value that satisfied the predicate
                                 //          in other words, until it finds the first value of the filtered stream.
    actual.toList
    stack should be (fullStack)
  }


  behavior of "append"

  it should "append an element to the end of an empty Stream" in {
    Empty.append(Stream(42)).toList should be (List(42))
  }

  it should "append an element to the end of a non-empty Stream" in {
    stream.append(Stream(42)).toList should be (fullStack :+ 42)
  }

  it should "be lazy in its parameter" in {
    val actual = stream.append(Stream(addToStack(42))) // note: not the stackingStream
    stack should be (emptyStack)
    actual.toList
    stack should be (List(42))
  }

  it should "be lazy in its evaluation" in {
    val actual = stackingStream.append(Stream(42))
    stack should be (List(1))
    actual.toList
    stack should be (fullStack)
  }


  behavior of "flatMap"

  it should "return empty when the Stream is empty" in {
    Empty.flatMap(a => Stream(a)).toList should be (Nil)
  }

  it should "flatMap over all the values" in {
    stream.flatMap(a => Stream(a.toString, a.toString)).toList should be (List("1", "1", "2", "2", "3", "3", "4", "4"))
  }

  it should "be lazy" in {
    val actual = stackingStream.flatMap(a => Stream(a.toString, a.toString))
    stack should be (List(1))
    actual.toList
    stack should be (fullStack)
  }


  behavior of "constant"

  it should "return a Stream of constants" in {
    Stream.constant(42).take(3).toList should be (List(42, 42, 42))
  }

  it should "be lazy" in {
    val actual = Stream.constant(addToStack(42)).take(3)
    stack should be (emptyStack)
    actual.toList
    stack should be (List(42, 42, 42))
  }


  behavior of "from"

  it should "return a Stream of increasing integers" in {
    Stream.from(3).take(3).toList should be (List(3, 4, 5))
  }

  it should "be lazy" in {
    val actual = Stream.from(addToStack(3)).take(3)
    stack should be (emptyStack)
    actual.toList
    stack should be (List(3, 3, 3))
  }


  behavior of "fibs"

  it should "return a Stream of fibonacci numbers" in {
    Stream.fibs.take(9).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21))
  }

  it should "be lazy" in {
    // I don't know how to test this
  }


  behavior of "unfold"

  it should "be tested in the subsequent tests" in {
    // see the tests for fibs2, from2, contant2 and ones2
  }


  behavior of "fibs in terms of unfold"

  it should "return a Stream of fibonacci numbers" in {
    Stream.fibs2.take(9).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21))
  }


  behavior of "from in terms of unfold"

  it should "return a Stream of increasing integers" in {
    Stream.from2(3).take(3).toList should be (List(3, 4, 5))
  }


  behavior of "constant in terms of unfold"

  it should "return a Stream of constants" in {
    Stream.constant2(42).take(3).toList should be (List(42, 42, 42))
  }


  behavior of "ones in terms of unfold"

  it should "return a Stream of 1s" in {
    Stream.ones2.take(3).toList should be (List(1, 1, 1))
  }
}
