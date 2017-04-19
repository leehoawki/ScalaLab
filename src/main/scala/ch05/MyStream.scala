package ch05

import ch03.MyList

sealed trait MyStream[+A] {
  def toList: MyList[A]

  def take(n: Int): MyStream[A]

  def drop(n: Int): MyStream[A]

  def takeWhile(p: A => Boolean): MyStream[A]
}

case object MyEmpty extends MyStream[Nothing] {
  override def toList: MyList[Nothing] = ???

  override def take(n: Int): MyStream[Nothing] = ???

  override def drop(n: Int): MyStream[Nothing] = ???

  override def takeWhile(p: (Nothing) => Boolean): MyStream[Nothing] = ???
}

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] {
  override def toList: MyList[A] = ???

  override def take(n: Int): MyStream[A] = ???

  override def drop(n: Int): MyStream[A] = ???

  override def takeWhile(p: (A) => Boolean): MyStream[A] = ???
}

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyCons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
