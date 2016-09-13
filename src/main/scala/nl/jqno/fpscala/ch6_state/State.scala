package nl.jqno.fpscala.ch6_state

case class State[S, +A](run: S => (A, S)) {

  // 6.10: map and flatMap
  def map[B](f: A => B): State[S, B] = ???
  def flatMap[B](f: A => State[S, B]): State[S, B] = ???
}

object State {

  // 6.10: map2 and sequence
  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = ???
  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = ???
}
