package nl.jqno.fpscala.ch11_monads

import nl.jqno.fpscala.ch9_parsers._
import nl.jqno.fpscala.ch8_testing._
import nl.jqno.fpscala.ch7_parallelism._
import nl.jqno.fpscala.ch7_parallelism.Par._
import nl.jqno.fpscala.ch6_state._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise 11.3: sequence & traverse
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List.empty[A]))((cur, acc) => map2(cur, acc)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B]))((cur, acc) => map2(f(cur), acc)(_ :: _))


  // Exercise 11.4: replicateM
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    map(ma)(a => List.fill(n)(a))


  // Exercise 11.6: filterM
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List.empty[A])) { (cur, acc) =>
      flatMap(acc) { macc => map(f(cur)) { b => if (b) cur :: macc else macc } }
    }


  // Exercise 11.7: Kleisli composition
  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)


  // Exercise 11.8: flatMap in terms of compose
  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit,A,B](_ => ma, f)(())


  // Exercise 11.9: flatMap and compose are equivalent
  /*
   *      flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
   *
   * === definition of compose
   *
   *      flatMap(compose(_ => x, f))(())(g) == flatMap(x)(a => compose(_ => f(a), g)(()))
   *
   * === definintion of compose
   *
   *      compose(_ => compose(_ => x, f)(()), g)(()) == compose(_ => x, a => compose(_ => f(a), g)(()))(())
   *
   * === apply ()
   *
   *      compose(() => compose(() => x, f), g) == compose(() => x, a => compose(() => f(a)))
   *
   * === substitute `() => x` with `q`
   *
   *      compose(() => compose(q, f), g) == compose(q, a => compose(() => f(a), g))
   *
   * === can we just drop the ()'s?
   *
   *      compose(compose(q, f), g) == compose(q, a => compose(f(a), g))
   *
   * === apply a
   *
   *      compose(compose(q, f), g) == compose(q, compose(f, g))
   *
   * === rename variables
   *
   *      compose(compose(f, g), h) == compose(f, compose(g, h))
   *
   */


  // Exercise 11.10: unit laws
  /* Left identity:
   *
   *      compose(unit, f) == f
   *
   * === definition of compose
   *    
   *      y => flatMap(unit(y))(f) == f
   *
   * === apply y
   *
   *      flatMap(unit(y))(f) == f(y)
   */
  /*
   * Right identity:
   *
   *      compose(f, unit) == f
   *
   * === definition of compose
   *
   *      a => flatMap(f(a))(unit) == f
   *
   * === apply a
   *
   *      flatMap(f(a))(unit) = f(a)
   *
   * === f(a) == x
   *
   *      flatMap(x)(unit) == x
   *
   */


  // Exercise 11.11: identity laws for Option
  /*
   * Left identity:
   *
   * // don't have to do None here because we don't have to nest the monads
   *
   *
   *      flatMap(Some(1))(f)
   *
   * === definition of flatMap
   *
   *      f(1)
   */
  /*
   * Right identity:
   *
   *      flatMap(None)(Some(_))
   *
   * === definition of flatMap
   *
   *      None
   *
   *
   *      flatMap(Some(1))(Some(_))
   *
   * === definition of flatMap
   *
   *      Some(1)
   */


  // Exercise 11.12: join
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)


  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = ???
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  // Exercise 11.1: monad instances
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  // Exercise 11.2: state monad
  // I'm sorry, but I peeked...
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)
  }

  //val idMonad: Monad[Id] = ???

  //def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = ???
  def flatMap[B](f: A => Id[B]): Id[B] = ???
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

