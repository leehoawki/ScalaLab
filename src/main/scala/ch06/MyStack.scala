package ch06

import ch03.{MyCons, MyList, MyNil}
import ch04.{MyNone, MyOption, MySome}

object MyStack {
  type MyStack = MyList[Int]

  def pop = MyState[MyStack, MyOption[Int]] {
    case MyCons(x, xs) => (MySome(x), xs)
    case MyNil => (MyNone, MyNil)
  }

  def push(i: Int) = MyState[MyStack, Unit] { xs => ((), MyCons(i, xs)) }
}
