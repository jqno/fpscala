package nl.jqno.fpscala

import scala.annotation.tailrec

object Chapter2 extends App {
  // 2.1: Fibonacci
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, acc: Int): Int =
      if (n == 0) prev
      else if (n == 1) acc
      else go(n - 1, acc, prev + acc)
    
    if (n < 0) throw new IllegalArgumentException(s"$n < 0")
    else go(n, 0, 1)
  }
}
