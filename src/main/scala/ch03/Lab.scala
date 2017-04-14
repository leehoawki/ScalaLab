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

  @tailrec
  def dropWhile2[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(h, t) => if (f(h)) dropWhile2(t)(f) else l
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(h, t) => MyCons(h, init(t))
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case MyCons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length[A](as: MyList[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  def length2[A](as: MyList[A]): Int = foldLeft(as, 0)((y, _) => y + 1)

  def sum(as: MyList[Int]): Int = foldLeft(as, 0)((x, y) => x + y)

  def products(as: MyList[Double]): Double = foldLeft(as, 1.0)((x, y) => x * y)

  def reverse[A](as: MyList[A]): MyList[A] = MyNil //TODO

  def foldLeft2[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = z //TODO

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = MyNil //TODO
}