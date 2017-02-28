package nl.jqno.fpscala.ch10_monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  
  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = List.empty[A]
  }


  // Exercise 10.1: monoids for Int and Boolean
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }


  // Exercise 10.2: a monoid for Options
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }
  def dualOptionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a2 orElse a1
    val zero = None
  }


  // Exercise 10.3: endoMonoid
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    val zero = (a: A) => a
  }
  def dualEndoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a2 andThen a1
    val zero = (a: A) => a
  }
}

object MonoidLaws extends App {
  import nl.jqno.fpscala.ch8_testing._
  import Prop._

  // Exercise 10.4: property-based testing
  def monoidGenerator[A]: Gen[(Monoid[A], A)] = ???
  def identityLaw[A] = forAll(monoidGenerator) { case (m, a) =>
    m.op(m.zero, a) == m.op(a, m.zero)
  }

  def associativityLaw[A] = forAll(monoidGenerator) { case (m, a) =>
    m.op(m.op(a, a), a) == m.op(a, m.op(a, a))
  }

  Prop.run(identityLaw)
  Prop.run(associativityLaw)
}
