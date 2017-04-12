package ch03

sealed trait MyList[+A]

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def product(ints: MyList[Double]): Double = ints match {
    case MyNil => 1
    case MyCons(0, _) => 0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))
}

object Lab {
  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, t) => t
  }

  def setHead[A](head: A, tail: MyList[A]): MyList[A] = MyCons(head, tail)

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    MyNil
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    MyNil
  }
}
