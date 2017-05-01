package nl.jqno.fpscala.ch14_localeffects

import org.scalatest.{FlatSpec, Matchers}

class LocalEffectsTest extends FlatSpec with Matchers {
  // Exercise 14.1: STArray.fill
  behavior of "STArray.fill"

  it should "fill an array based on the content of a map" in {
    val map = Map(0 -> "a", 2 -> "b")
    val actual = new RunnableST[List[String]] {
      def apply[S] = for {
        a <- STArray(4, "x")
        _ <- a.fill(map)
        l <- a.freeze
      } yield l
    }
    ST.runST(actual) should be (List("a", "x", "b", "x"))
  }


  // Exercise 14.2: Quicksort
  behavior of "quicksort"

  it should "sort a list" in {
    Immutable.quicksort(List(3, 1, 4, 2)) should be (List(1, 2, 3, 4))
  }
}
