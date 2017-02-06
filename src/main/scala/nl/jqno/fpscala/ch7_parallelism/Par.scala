import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  // 7.2: representation
  // We can have an ADT with two case classes that implement the Par trait:
  // one for unit, which is strict, and one for fork, which isn't.
  // run could pattern match over it, and present the Fork to an executor service.
  // We could also wrap it in a Future, but then we'd probably have to await.
  // Do we want that!?


  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit) = get
    override def isCancelled = false
    override def cancel(evenIfRunning: Boolean) = false
  }

  def map2noTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  // Exercise 7.1: map2 signature
  // Exercise 7.3: map2 with timeout
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(timeout: Long, units: TimeUnit): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  // I got as far as the case class, with the correct constructor signature
  // and all methods filled in except both `get` overloads.
  // Then I cheated and looked at the answer :-O
  private case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    override def isDone = cache.isDefined
    override def get = compute(Long.MaxValue)
    override def get(timeout: Long, units: TimeUnit) = compute(TimeUnit.NANOSECONDS.convert(timeout, units))
    override def isCancelled = af.isCancelled || bf.isCancelled
    override def cancel(evenIfRunning: Boolean) = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = af.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = bf.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }


  // Exercise 7.4: asyncF
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // Exercise 7.5: sequence
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((cur, acc) => map2noTimeout(cur, acc)((c, a) => c :: a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.6: parFilter
  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2noTimeout(a, unit(())) { case (a, _) => f(a) }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[List[A]]] =
      as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  // Exercise 7.7: map(map(y)(g))(f) == map(y)(f compose g)
  // skip :/
  
  // Exercise 7.8: bugs with ExecutorServices
  // With a single-threaded ExecutorService, you get deadlock if you fork inside a fork.

  // Exercise 7.9: bugs with fixed-size threadpools
  // Just nest the forks deeply enough.

  // Exercise 7.10: handling errors
  // I'm skipping this one for now.

  // Exercise 7.11: choiceN & choice
  def choiceN0[A](n: Par[Int], choices: List[Par[A]]): Par[A] = es => {
    val choice = run(es)(n).get
    if (choice < 0 || choice >= choices.size)
      throw new IndexOutOfBoundsException
    choices(choice)(es)
  }

  def choice0[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1), List(t, f))


  // Exercise 7.12: choiceMap
  def choiceMap0[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
    val choice = run(es)(key).get
    if (!choices.contains(choice))
      throw new NoSuchElementException
    choices(choice)(es)
  }


  // Exercise 7.13: chooser
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    run(es)(choices(a))
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int], choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    chooser(key)(choices)
}
