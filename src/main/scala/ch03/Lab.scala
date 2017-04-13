package ch03

import scala.annotation.tailrec

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

  @tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else l match {
      case MyNil => MyNil
      case MyCons(_, t) => drop(t, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) => if (f(h)) dropWhile(t, f) else l
  }
}
