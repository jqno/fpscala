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

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call = a(es).get
    })


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
    a => fork(unit(f(a)))
}
