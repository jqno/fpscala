package nl.jqno.fpscala

import scala.annotation.tailrec

object Chapter3 {
  // 3.1: pattern match
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    //    case Cons(h, t) => h + sum(t)  // doesn't compile
    case _ => 101
  }

  // 3.2: tail
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new IllegalStateException("Can't tail a Nil")
    case Cons(_, t) => t
  }

  // 3.3: setHead
  def setHead[A](h: A, as: List[A]): List[A] = Cons(h, tail(as))


  // 3.4: drop
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case 0 => as
    case i => drop(tail(as), i - 1)
  }


  // 3.5: dropWhile
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, tail) if f(h) => dropWhile(tail, f)
    case xs => xs
  }


  // 3.6: init
  def init[A](as: List[A]): List[A] = as match {
    case Nil => throw new IllegalStateException("Can't init a Nil")
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }


  // foldRight
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }


  // 3.7: short-circuit product
  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)
  // can't be short-circuited if 0.0 is encountered,
  // because both parameters to f are fully evaluated before f itself is called.


  // 3.8: foldRight identity
  val ex3_8 = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  // foldRight(List(1, 2, 3), Nil)(Cons(_, _))
  // Cons(1, foldRight(List(2, 3), Nil)(Cons(_, _)))
  // Cons(1, Cons(2, foldRight(List(3), Nil)(Cons(_, _))))
  // Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil)(Cons(_, _)))))
  // Cons(1, Cons(2, Cons(3, Nil)))


  // 3.9: length
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)


  // 3.10: foldLeft
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }


  // 3.11: sum, product, left via foldLeft
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def productLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  def lengthLeft(as: List[Int]): Int = foldLeft(as, 0)((acc, _) => acc + 1)


  // 3.12: reverse
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))


  // 3.13: foldLeft via foldRight and vice versa
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???


  // 3.14: append
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)(Cons(_, _))


  // 3.15: flatten
  def flatten[A](xss: List[List[A]]): List[A] = foldRight(xss, Nil: List[A])(append)


  // 3.16: addOne
  def addOne(as: List[Int]): List[Int] = {
    @tailrec
    def go(acc: List[Int], xs: List[Int]): List[Int] = xs match {
      case Nil => reverse(acc)
      case Cons(h, t) => go(Cons(h + 1, acc), t)
    }
    go(Nil, as)
  }


  // 3.17: doublesToStrings
  def doublesToStrings(ds: List[Double]): List[String] = {
    @tailrec
    def go(acc: List[String], xs: List[Double]): List[String] = xs match {
      case Nil => reverse(acc)
      case Cons(h, t) => go(Cons(h.toString, acc), t)
    }
    go(Nil, ds)
  }


  // 3.18: map
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    @tailrec
    def go(acc: List[B], xs: List[A]): List[B] = xs match {
      case Nil => reverse(acc)
      case Cons(h, t) => go(Cons(f(h), acc), t)
    }
    go(Nil, as)
  }


  // 3.19: filter
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def go(acc: List[A], xs: List[A]): List[A] = xs match {
      case Nil => reverse(acc)
      case Cons(h, t) if f(h) => go(Cons(h, acc), t)
      case Cons(_, t) => go(acc, t)
    }
    go(Nil, as)
  }


  // 3.20: flatMap
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    @tailrec
    def go(acc: List[B], xs: List[A]): List[B] = xs match {
      case Nil => reverse(acc)
      case Cons(h, t) => go(append(f(h), acc), t)
    }
    go(Nil, as)
  }


  // 3.21: flatMapFilter
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
