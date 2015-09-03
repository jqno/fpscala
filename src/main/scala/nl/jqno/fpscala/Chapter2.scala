package nl.jqno.fpscala

import scala.annotation.tailrec

object Chapter2 extends App {
  // 2.1: Fibonacci
//  def fib(n: Int): Int = {
//    @tailrec
//    def go(n: Int, prev: Int, acc: Int): Int =
//      if (n == 0) prev
//      else if (n <= 1) acc
//      else go(n - 1, acc, prev + acc)
//
//    if (n < 0) throw new IllegalArgumentException(s"$n < 0")
//    else go(n, 0, 1)
//  }

  def fib(n: Int): Int = {
    @tailrec
    def go(c: Int, acc: Int, curr: Int): Int =
      if (c == n) acc
      else go(c + 1, curr, acc + curr)

    if (n < 0) throw new IllegalArgumentException(s"$n < 0")
    else go(0, 0, 1)
  }

  
  // 2.2: isSorted
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean =
      if (i >= as.length - 1) true
      else ordered(as(i), as(i + 1)) && go(i + 1)

    go(0)
  }

//  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
//    as.sliding(2) forall { case Array(x, y) => ordered(x, y) }
  
  
  // 2.3: currying
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)
  
  
  // 2.4: uncurrying
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
  
  
  // 2.5: compose
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
