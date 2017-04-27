package ch06

import ch03.MyList
import ch05.MyStream
import org.scalatest.{FlatSpec, Matchers}

class MyStateSpec extends FlatSpec with Matchers {
  "MyState" should "be correct" in {
    type NewRand[A] = MyState[MyRNG, A]

    def int: NewRand[Int] = MyState(rng => rng.nextInt)

    def ints(count: Int): NewRand[MyList[Int]] = MyState.sequence(MyStream.constant(int).take(count).toList)

    int.flatMap(x => int.flatMap(y => ints(x % 10).map(xs => MyList.map(xs)(_ % y))))
  }
}