package nl.jqno.fpscala.ch6_state

import org.scalatest.{FlatSpec, Matchers}
import CandyDispenser.simulateMachine

class CandyDispenserTest extends FlatSpec with Matchers {

  val locked = true
  val unlocked = false
  val oneTurn = List(Turn)
  val oneCoin = List(Coin)
  val lockedAndLoaded = Machine(locked, 10, 5)
  val unlockedAndLoaded = Machine(unlocked, 10, 5)


  behavior of "Machine"

  it should "do the example from the book" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val actual = simulateMachine(inputs).run(lockedAndLoaded)
    actual._1 should be ((14, 1))
  }

  it should "unlock when a coin is inserted and there's candy left" in {
    val actual = simulateMachine(oneCoin).run(lockedAndLoaded)
    actual._1 should be ((11, 5))
    actual._2.locked should be (unlocked)
  }

  it should "dispense a candy and become locked when it's unlocked and gets a turn" in {
    val actual = simulateMachine(oneTurn).run(unlockedAndLoaded)
    actual._1 should be ((10, 4))
    actual._2.locked should be (locked)
  }

  it should "ignore a turn if it's locked" in {
    val actual = simulateMachine(oneTurn).run(lockedAndLoaded)
    actual._2 should be (lockedAndLoaded)
  }

  it should "ignore a coin it it's unlocked" in {
    val actual = simulateMachine(oneCoin).run(unlockedAndLoaded)
    actual._2 should be (unlockedAndLoaded)
  }

  it should "ignore a turn if it's unlocked but out of candy" in {
    val initial = Machine(unlocked, 1, 0)
    val actual = simulateMachine(oneTurn).run(initial)
    actual._2 should be (initial)
  }

  it should "ignore a coin if it's locked but out of candy" in {
    val initial = Machine(locked, 1, 0)
    val actual = simulateMachine(oneCoin).run(initial)
    actual._2 should be (initial)
  }
}
