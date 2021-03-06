package nl.jqno.fpscala.ch12_applicative

import nl.jqno.fpscala.ch11_monads.Functor
import nl.jqno.fpscala.ch6_state._
import StateUtil._ // defined at bottom of this file
import nl.jqno.fpscala.ch10_monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  self =>

  // Exercise 12.2: apply, map2
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ apply _)    // that's Function.apply

  def unit[A](a: => A): F[A]


  // Exercise 12.1: sequence, replicateM, product
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))


  // Exercise 12.3: map3, map4
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbcd = map(fa)(f.curried)
    val fcd = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fbcde = map(fa)(f.curried)
    val fcde = apply(fbcde)(fb)
    val fde = apply(fcde)(fc)
    apply(fde)(fd)
  }


  // Exercise 12.8: product
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))
      override def apply[A,B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }


  // Exercise 12.8: another compose
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))
      override def apply[A,B](fgab: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fgab, fga)(G.map2(_, _)(_ apply _))
    }


  // Exercise 12.12: sequenceMap
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc) => map2(fv, acc)((v, m) => m + (k -> v)) }



  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  // Exercise 12.5: either monad
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] =
      Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.right.flatMap(f)
  }


  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }


  // Exercise 12.6: validation applicative
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] =
      Success(a)
    override def map2[A,B,C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Success(a), Failure(h, t)) => Failure(h, t)
        case (Failure(h, t), Success(b)) => Failure(h, t)
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (t2 :+ h1))
      }
  }



  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)


  // Exercise 12.14: map in terms of traverse
  case class Identity[A](value: A)
  implicit val identityApplicative = new Applicative[Identity] {
    override def unit[A](a: => A): Identity[A] =
      Identity(a)
    override def map2[A,B,C](a: Identity[A], b: Identity[B])(f: (A, B) => C): Identity[C] =
      Identity(f(a.value, b.value))
  }
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(f andThen Identity.apply).value
  


  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1


  // Exercise 12.16: reverse
  // OK, this thing typechecks, but it's (probably) incorrect.
  // I can't really run it, because I'm getting a StackOverflowError if I run it that I don't care to debug. :/
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, ())((a,s) => (a,s))._1


  // Exercise 12.17: foldLeft in terms of mapAccum
  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2


  override def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???


  // Exercise 12.19: compose
  // I peeked most of this...
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[H[_]:Applicative,A,B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      self.traverse(fa)(G.traverse(_)(f))
  }

}


// Exercise 12.13: Traverse instances for List, Option and Tree
object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_]:Applicative,A,B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldRight(G.unit(List.empty[B]))((cur, acc) => G.map2(f(cur), acc)(_ :: _))
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_]:Applicative,A,B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      fa.foldRight(G.unit(None: Option[B]))((cur, _) => G.map(f(cur))(b => Some(b)))
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_]:Applicative,A,B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      // ok...I peeked
      G.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

