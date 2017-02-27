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


// Exercise 9.13: Yeah, I think I'm just going to stop here. I've already
// skipped many of the previous exercises because I don't enjoy the vagueness
// of big, open-ended design exercises at this point.
//
// Also, the answerKey stopped being helpful a few exercises ago, and the fully
// implemented answer has diverged from this point in the book to be really
// helpful anymore, either. The answerKey even suggest that I read on instead
// of look at the reference implementation, so I'll just do that, and maybe
// come back to these exercises when I've read the rest of the chapter. Or
// book.
//
// Bad excuses, I guess, but I'm doing this book for fun so I should be having
// fun instead of being frustrated by the answerKey, right? Anyway, I have high
// hopes for the next chapters.

