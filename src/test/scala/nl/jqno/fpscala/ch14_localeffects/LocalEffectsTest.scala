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


  // Exercise 14.3: mutable HashMap
  behavior of "mutable HashMap"

  it should "have HashMappy operations" in {
    val original = Map(1 -> "a", 2 -> "b", 4 -> "d")
    val actual = new RunnableST[Map[Int, String]] {
      def apply[S] = for {
        m <- STMap.fromMap(original)
        a <- m.read(1)
        _ <- m.write(1, a + a)
        _ <- m.write(3, "c")
        r <- m.freeze
      } yield r
    }
    ST.runST(actual) should be (Map(1 -> "aa", 2 -> "b", 3 -> "c", 4 -> "d"))
  }
}

