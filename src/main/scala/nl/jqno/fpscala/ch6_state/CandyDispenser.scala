package nl.jqno.fpscala.ch6_state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, coins: Int, candies: Int)

object CandyDispenser {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
    val initial = ((machine.coins, machine.candies), machine)
    inputs.foldLeft(initial)((acc, curr) => simulateSingleInput(curr).run(acc._2))
  }

  private def simulateSingleInput(input: Input): State[Machine, (Int, Int)] = State { machine =>
    val s = input match {
      case Coin if machine.locked && machine.candies > 0 =>
        machine.copy(locked = false, coins = machine.coins + 1)
      case Turn if !machine.locked && machine.candies > 0 =>
        machine.copy(locked = true, candies = machine.candies - 1)
      case _ =>
        machine
    }
    ((s.coins, s.candies), s)
  }
}
