package nl.jqno.fpscala

import org.scalatest.{Matchers, FlatSpec}

class Chapter4OptionTest extends FlatSpec with Matchers {
  val some: Option[Int] = Some(1)
  val some2: Option[Int] = Some(2)
  val none: Option[Int] = None
  val even = (i: Int) => i % 2 == 0
  val evenOpt = (i: Int) => if (even(i)) Some(i) else None


  behavior of "map"

  it should "map a Some" in {
    some.map(_ + 1) should be (Some(2))
  }

  it should "map a None" in {
    none.map(_ + 1) should be (None)
  }


  behavior of "flatMap"

  it should "flatMap a Some" in {
    some2.flatMap(evenOpt) should be (Some(2))
    some.flatMap(evenOpt) should be (None)
  }

  it should "flatMap a None" in {
    none.flatMap(evenOpt) should be (None)
  }


  behavior of "getOrElse"

  it should "return the value if it's a Some" in {
    some.getOrElse(42) should be (1)
  }

  it should "return the default value if it's a None" in {
    none.getOrElse(42) should be (42)
  }


  behavior of "orElse"

  it should "return itself if it's a Some" in {
    some.orElse(some2) should be (some)
  }

  it should "return the default value if it's a None" in {
    none.orElse(some2) should be (some2)
  }


  behavior of "filter"

  it should "return itself if the predicate is true" in {
    some2.filter(even) should be (some2)
  }

  it should "return None if the predicate is false" in {
    some.filter(even) should be (None)
  }
}