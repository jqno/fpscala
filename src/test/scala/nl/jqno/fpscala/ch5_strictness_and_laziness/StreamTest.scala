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
}
