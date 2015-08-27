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
  
  
  behavior of "2.2: isSorted"
  
  it should "say an empty or 1-element array is sorted" in {
    isSorted(Array(), (_: Int, _: Int) => false) should be (true)
    isSorted(Array("hello"), (_: String, _: String) => false) should be (true)
  }
  
  it should "recognise a sorted array of ints" in {
    val xs = Array(0, 1, 1, 2, 3, 5, 8, 13, 21)
    isSorted(xs, (x: Int, y: Int) => x <= y) should be (true)
  }
  
  it should "recognise an unsorted array of ints" in {
    val xs = Array(1337, 1, 42)
    isSorted(xs, (x: Int, y: Int) => x <= y) should be (false)
  }
  
  it should "recognise an array that is sorted except for the last element" in {
    val xs = Array(42, 1337, 1)
    isSorted(xs, (x: Int, y: Int) => x <= y) should be (false)
  }
  
  
  behavior of "2.3: currying"
  
  it should "curry" in {
    val f = (a: Int, b: Int) => a + b
    val g = curry(f)(2)
    g(3) should be (f(2, 3))
  }
}
