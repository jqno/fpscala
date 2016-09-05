package nl.jqno.fpscala.ch5_strictness_and_laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {
  val stream = Stream(1, 2, 3, 4)

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


  behavior of "drop"

  it should "stop when empty" in {
    Empty.drop(2).toList should be (Nil)
    stream.drop(5).toList should be (Nil)
  }

  it should "drop the first elements of a stream" in {
    stream.drop(2).toList should be (List(3, 4))
  }
}
