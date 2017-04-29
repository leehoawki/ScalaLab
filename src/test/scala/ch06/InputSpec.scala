package ch06

import ch03.MyList
import org.scalatest.{FlatSpec, Matchers}

class InputSpec extends FlatSpec with Matchers {
  "SimulateMachine" should "be correct" in {
    val inputs = MyList(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Coin, Turn)
    Input.simulateMachine(inputs).run(Machine(true, 3, 0)) shouldEqual(3, Machine(true, 0, 5))

    val inputs2 = MyList(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    Input.simulateMachine(inputs2).run(Machine(true, 5, 10)) shouldEqual(4, Machine(true, 1, 14))
  }
}