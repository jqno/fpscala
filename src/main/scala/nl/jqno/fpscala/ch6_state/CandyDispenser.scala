package nl.jqno.fpscala.ch6_state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, coins: Int, candies: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
    val next = inputs.foldLeft(machine)((acc, curr) => simulateSingleInput(curr)(acc))
    ((next.coins, next.candies), next)
  }

  private def simulateSingleInput(input: Input): Machine => Machine = machine => {
    input match {
      case Coin if machine.locked && machine.candies > 0 =>
        machine.copy(locked = false, coins = machine.coins + 1)
      case Turn if !machine.locked && machine.candies > 0 =>
        machine.copy(locked = true, candies = machine.candies - 1)
      case _ =>
        machine
    }
  }
}
