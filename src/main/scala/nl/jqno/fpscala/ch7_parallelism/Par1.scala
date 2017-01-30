package nl.jqno.fpscala.ch7_parallelism

trait Par[A]
object Par {
  // 7.1: map2 signature
  def map2[A, B, C](left: Par[A], right: Par[B])(f: (A, B) => C): Par[C] = ???
}

