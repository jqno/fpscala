package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
import nl.jqno.fpscala.ch8_testing._
import nl.jqno.fpscala.ch8_testing.Prop._
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  // Exercise 9.1: map2 and many1
  // def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
  //   product(p1, p2) map f.tupled

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)


  // Exercise 9.3: many in terms of or, map2, succeed
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List.empty[A])


  // Exercise 9.4: listOfN in terms of map2, succeed
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List.empty[A])
    else map2(p, listOfN(n - 1, p))(_ :: _)


  // Exercise 9.6: context-sensitive
  def contextSensitive: Parser[List[Char]] =
    "[0-9]".r flatMap (n => listOfN(n.toInt, char('a')))
  implicit def regex(r: Regex): Parser[String]


  // Exercise 9.7: product and map2 in terms of flatMap
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap(a => p2.flatMap(b => succeed((a, b))))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.flatMap(b => succeed(f(a, b))))


  // Exercise 9.8: map in terms of flatMap and other combinators
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))


  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  case class ParserOps[A](p: Parser[A]) {
    def | [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def ** [B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))


    // Exercise 9.2: laws for product
    // There are some problems with this one. But I was close!
    // See https://github.com/fpinscala/fpinscala/blob/master/answerkey/parsing/02.answer.scala
    def productLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[B])(in: Gen[String]): Prop =
      equal(p1 ** (p2 ** p3), (p1 ** p2) ** p3)(in) &&
      equal((p1 ** p2) map (t => t), (p1 map (a => a)) ** (p2 map (b => b)))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

