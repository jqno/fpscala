package nl.jqno.fpscala.ch7_parallelism

trait Par[A]
object Par {
  // 7.1: map2 signature
  def map2[A, B, C](left: Par[A], right: Par[B])(f: (A, B) => C): Par[C] = ???

  // 7.2: representation
  // We can have an ADT with two case classes that implement the Par trait:
  // one for unit, which is strict, and one for fork, which isn't.
  // run could pattern match over it, and present the Fork to an executor service.
  // We could also wrap it in a Future, but then we'd probably have to await.
  // Do we want that!?
}

