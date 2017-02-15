package nl.jqno.fpscala.ch8_testing

import Prop._
import SGen._

object Usage extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  Prop.run(maxProp)
}

