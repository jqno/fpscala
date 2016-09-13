package nl.jqno.fpscala.ch6_state

case class State[S, +A](run: S => (A, S)) {

  // 6.10: map and flatMap
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

object State {

  // 6.10: map2 and sequence
  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sa.flatMap(a => sb.map(b => f(a, b)))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List.empty))((curr, acc) => map2(curr, acc)(_ :: _))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
