package nl.jqno.fpscala.ch6_state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
