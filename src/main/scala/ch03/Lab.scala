package ch03

sealed trait xList[+A]

case object xNil extends xList[Nothing]

case class xCons[+A](head: A, tail: xList[A]) extends xList[A]

object xList {
  def sum(ints: xList[Int]): Int = ints match {
    case xNil => 0
    case xCons(x, xs) => x + sum(xs)
  }

  def product(ints: xList[Double]): Double = ints match {
    case xNil => 1
    case xCons(0, _) => 0
    case xCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): xList[A] =
    if (as.isEmpty) xNil
    else xCons(as.head, apply(as.tail: _*))
}

object Lab {

}