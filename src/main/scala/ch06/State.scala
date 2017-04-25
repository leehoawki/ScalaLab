package ch06

import ch03.{MyCons, MyList, MyNil}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] {
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a => sb.flatMap { b => State.unit(f(a, b)) } }
}

object State {
  def unit[S, A](a: A) = State[S, A](s => (a, s))

  def sequence[S, A](fs: MyList[State[S, A]]): State[S, MyList[A]] = MyList.foldRight(fs, unit(MyNil): State[S, MyList[A]])((a, b) => {
    a.map2(b)((x, y) => MyCons(x, y))
  })
}