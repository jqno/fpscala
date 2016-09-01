package nl.jqno.fpscala.ch4_handling_errors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(value => if (f(value)) Some(value) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionFunctions {
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap { m =>
    mean(xs.map(x => math.pow(x - m, 2)))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(_a => b.map(_b => f(_a, _b)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(head) :: tail => sequence(tail).map(head :: _)
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((curr, acc) => f(curr).flatMap(h => acc.map(t => h :: t)))
}
