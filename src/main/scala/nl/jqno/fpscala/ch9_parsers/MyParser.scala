package nl.jqno.fpscala.ch9_parsers

import scala.language.implicitConversions
import MyParserWrapper._

object MyParserWrapper {
  type MyParser[+A] = Location => Result[A]
}

object MyParsers extends Parsers[MyParser] {
  def string(s: String): MyParser[String] = ???
  def regex(r: scala.util.matching.Regex): MyParser[String] = ???
  def slice[A](p: MyParser[A]): MyParser[String] = ???
  def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???
  def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???
  def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???
  def attempt[A](p: MyParser[A]): MyParser[A] = ???
  def or[A](s1: MyParser[A],s2: => MyParser[A]): MyParser[A] = ???
}

trait Result[+A]
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

