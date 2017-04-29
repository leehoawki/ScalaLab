package ch06

import ch03.{MyCons, MyList, MyNil}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Input {
  def transition(input: Input, machine: Machine): Machine = {
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(_, nCandy, nCoin)) => Machine(false, nCandy, nCoin + 1)
      case (Turn, Machine(false, nCandy, nCoin)) => Machine(true, nCandy - 1, nCoin)
    }
  }

  def execute(inputs: MyList[Input], machine: Machine): Machine = {
    inputs match {
      case MyNil => machine
      case MyCons(h, t) => execute(t, transition(h, machine))
    }
  }

  def get[S]: MyState[S, S] = MyState[S, S] { s => (s, s) }

  def set[S](s: S): MyState[S, Unit] = MyState[S, Unit] { _ => ((), s) }

  def simulateMachine(inputs: MyList[Input]): MyState[Machine, Int] = {
    for {
      s0 <- get
      _ <- set(execute(inputs, s0))
      s1 <- get
    } yield s0.candies - s1.candies
  }
}