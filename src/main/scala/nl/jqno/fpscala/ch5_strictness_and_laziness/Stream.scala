package nl.jqno.fpscala.ch5_strictness_and_laziness

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {
  // 5.1: toList
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }


  // 5.2: take and drop
  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => empty
    case Empty => empty
    case Cons(h, t) => cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case _ if n == 0 => this
    case Empty => this
    case Cons(h, t) => t().drop(n - 1)
  }


  // 5.3: takeWhile
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }


  // taken from the book
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


  // 5.4: forAll
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((curr, acc) => p(curr) && acc)


  // 5.5: takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((curr, acc) => if (p(curr)) cons(curr, acc) else empty)


  // 5.6: headOption using foldRight
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))


  // 5.7: map, filter, append, flatMap
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (f(h)) cons(h, t) else t)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight[Stream[AA]](s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty)((h, t) => f(h).append(t))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  // 5.8: constant
  def constant[A](a: => A): Stream[A] = cons(a, constant(a))


  // 5.9: from
  def from(n: => Int): Stream[Int] = cons(n, from(n + 1))


  // 5.10: fibonacci
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))

    go(0, 1)
  }
}
