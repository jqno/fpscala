package nl.jqno.fpscala.ch8_testing

import Prop._
import SGen._

object Usage extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)


  // Exercise 8.14: sorted
  val sortedProp = forAll(listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.sliding(2).forall { _ match {
      case a :: b :: Nil => a <= b
      case _ => true
    }}
  }

  Prop.run(sortedProp)
}

