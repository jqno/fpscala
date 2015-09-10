package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}
import Chapter3._

class Chapter3Test extends FlatSpec with Matchers {
  val someList = List(1, 2, 3, 4, 5, 6)

  behavior of "pattern matching"

  it should "be 3" in {
    Chapter3.x should be (3)
  }


  behavior of "tail"

  it should "take the tail of a list" in {
    tail(someList) should be (List(2, 3, 4, 5, 6))
  }

  it should "throw when tailing a Nil" in {
    intercept[IllegalStateException] {
      tail(Nil)
    }
  }


  behavior of "setHead"

  it should "replace the head of a list" in {
    setHead(42, someList) should be (List(42, 2, 3, 4, 5, 6))
  }

  it should "throw when setHeading a Nil" in {
    intercept[IllegalStateException] {
      setHead(0, Nil)
    }
  }


  behavior of "drop"

  it should "drop the first n elements from a list" in {
    drop(someList, 2) should be (List(3, 4, 5, 6))
    drop(someList, 0) should be (someList)
  }


  behavior of "dropWhile"

  it should "drop the elements while something is true" in {
    dropWhile(someList, (x: Int) => x < 4) should be (List(4, 5, 6))
    dropWhile(Nil, (x: Int) => x < 4) should be (Nil)
  }


  behavior of "init"

  it should "give all the elements from a list, except the last" in {
    init(List(1)) should be (Nil)
    init(someList) should be (List(1, 2, 3, 4, 5))
  }

  it should "throw when initing a Nil" in {
    intercept[IllegalStateException] {
      init(Nil)
    }
  }


  behavior of "foldRight identity"

  it should "be the identity function" in {
    ex3_8 should be (List(1, 2, 3))
  }


  behavior of "length"

  it should "give the correct length" in {
    Chapter3.length(someList) should be (6)
  }


  behavior of "foldLeft"

  it should "sum with foldLeft" in {
    foldLeft(someList, 0)(_ + _) should be (21)
  }

  it should "product with foldLeft" in {
    foldLeft(someList, 1)(_ * _) should be (720)
  }

  it should "reverse with foldLeft" in {
    foldLeft(someList, Nil: List[Int])((xs, x) => Cons(x, xs)) should be (List(6, 5, 4, 3, 2, 1))
  }


  behavior of "sum, product and length via foldLeft"

  it should "sum" in {
    sumLeft(someList) should be (21)
  }

  it should "product" in {
    productLeft(someList) should be (720)
  }

  it should "length" in {
    lengthLeft(someList) should be (6)
  }


  behavior of "reverse"

  it should "reverse" in {
    reverse(someList) should be (List(6, 5, 4, 3, 2, 1))
  }


  behavior of "foldLeft via foldRight"

  ignore should "have the same result" in {
    foldLeft2(someList, 0)(_ + _) should be (foldLeft(someList, 0)(_ + _))
    foldLeft2(someList, 1)(_ * _) should be (foldLeft(someList, 1)(_ * _))
    foldLeft2(someList, Nil: List[Int])((xs, x) => Cons(x, xs)) should be (foldLeft(someList, Nil: List[Int])((xs, x) => Cons(x, xs)))
  }


  behavior of "foldRight via foldLeft"

  ignore should "have the same result" in {
    foldRight2(someList, 0)(_ + _) should be (foldRight(someList, 0)(_ + _))
    foldRight2(someList, 1)(_ * _) should be (foldRight(someList, 1)(_ * _))
    foldRight2(someList, Nil: List[Int])(Cons(_, _)) should be (foldRight(someList, Nil: List[Int])(Cons(_, _)))
  }


  behavior of "append"

  it should "append two lists" in {
    append(someList, someList) should be (List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6))
  }


  behavior of "flatten"

  it should "flatten a list" in {
    flatten(List(List(1, 2, 3), List(4), List(5, 6))) should be (someList)
  }


  behavior of "addOne"

  it should "add one to every element of a list" in {
    addOne(someList) should be (List(2, 3, 4, 5, 6, 7))
  }


  behavior of "doublesToStrings"

  it should "convert a list of doubles to a list of strings" in {
    doublesToStrings(List(1.0, 4.2, 1.337)) should be (List("1.0", "4.2", "1.337"))
  }


  behavior of "map"

  it should "do mappy things" in {
    map(someList)(_ + 1) should be (List(2, 3, 4, 5, 6, 7))
    map(List(1.0, 4.2, 1.337))(_.toString) should be (List("1.0", "4.2", "1.337"))
  }


  behavior of "filter"

  it should "filter out all odd numbers" in {
    filter(someList)(_ % 2 == 0) should be (List(2, 4, 6))
  }


  behavior of "flatMap"

  it should "apply a function and flatten" in {
    flatMap(someList)(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
  }
}
