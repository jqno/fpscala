package nl.jqno.fpscala

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) if f(value) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
