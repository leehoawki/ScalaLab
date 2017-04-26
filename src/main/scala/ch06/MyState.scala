package ch06

import ch03.{MyCons, MyList, MyNil}

case class MyState[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = MyState[S, B] {
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  }

  def map[B](f: A => B): MyState[S, B] = flatMap { a => MyState.unit(f(a)) }

  def map2[B, C](sb: MyState[S, B])(f: (A, B) => C): MyState[S, C] = flatMap { a => sb.flatMap { b => MyState.unit(f(a, b)) } }
}

object MyState {
  def unit[S, A](a: A) = MyState[S, A](s => (a, s))

  def sequence[S, A](fs: MyList[MyState[S, A]]): MyState[S, MyList[A]] = MyList.foldRight(fs, unit(MyNil): MyState[S, MyList[A]])((a, b) => {
    a.map2(b)((x, y) => MyCons(x, y))
  })
}