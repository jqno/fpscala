package nl.jqno.fpscala

import org.scalatest.{FlatSpec, Matchers}
import Chapter2._

class Chapter2Test extends FlatSpec with Matchers {
  behavior of "2.1: Fibonacci"
  
  it should "return the correct edge cases" in {
    intercept[IllegalArgumentException] {
      fib(-1)
    }
  }
  
  it should "return the seed values" in {
    fib(0) should be (0)
    fib(1) should be (1)
  }
  
  it should "return the correct values" in {
    fib(2) should be (1)
    fib(6) should be (8)
    fib(8) should be (21)
  }
}
