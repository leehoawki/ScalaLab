package ch06

import ch03.MyList

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Input {
  def simulateMachine(inputs: MyList[Input]): State[Machine, (Int, Int)] = ???
}