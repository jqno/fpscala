package nl.jqno.fpscala.ch5_strictness_and_laziness

import Stream._

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
}
