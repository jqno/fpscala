package nl.jqno.fpscala.ch8_testing

import nl.jqno.fpscala.ch7_parallelism.Par
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


  val p2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2))
  }

  Prop.run(p2)


  // Exercise 8.17: fork(x) == x
  val forkProp = forAllPar(smallInt) { n =>
    equal(
      Par.fork(Par.unit(n)),
      Par.unit(n))
  }

  Prop.run(forkProp)
  StupidThreadPools.shutdown()
}

