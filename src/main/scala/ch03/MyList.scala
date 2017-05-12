package ch03

import ch03.MyList.foldRight

import scala.annotation.tailrec

sealed trait MyList[+A] {
  def map[B](f: A => B): MyList[B] = foldRight(this, MyNil: MyList[B])((x, y) => MyCons(f(x), y))

  def filter(f: A => Boolean): MyList[A] = foldRight(this, MyNil: MyList[A])((x, y) => if (f(x)) MyCons(x, y) else y)

  def append[B >: A](a: MyList[B]): MyList[B] = foldRight(this, a)((x, y) => MyCons(x, y))

  def index(n: Int): A = {
    def go(count: Int, list: MyList[A]): A = (count, list) match {
      case (_, MyNil) => throw new IndexOutOfBoundsException()
      case (c, MyCons(h, t)) if c > 0 => go(c - 1, t)
      case (c, MyCons(h, t)) if c < 0 => throw new IndexOutOfBoundsException()
      case (0, MyCons(h, t)) => h
    }

    go(n, this)
  }
}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def sort(ints: MyList[Int]): MyList[Int] = {
    def qs(list: MyList[Int]): MyList[Int] = list match {
      case MyNil => MyNil
      case MyCons(x, xs) => qs(xs.filter(_ < x)).append(MyList(x)).append(xs.filter(_ >= x))
    }

    qs(ints)
  }

  def product(ints: MyList[Double]): Double = ints match {
    case MyNil => 1
    case MyCons(0, _) => 0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

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

  def take[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) MyNil
    else l match {
      case MyNil => MyNil
      case MyCons(x, t) => MyCons(x, take(t, n - 1))
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

  def length[A](as: MyList[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case MyCons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    def step(x: A, g: B => B)(b: B) = g(f(b, x))

    val id: B => B = x => x

    foldRight(as, id)(step)(z)
  }

  def length2[A](as: MyList[A]): Int = foldLeft2(as, 0)((y, _) => y + 1)

  def sum2(as: MyList[Int]): Int = foldLeft2(as, 0)(_ + _)

  def product2(as: MyList[Double]): Double = foldLeft2(as, 1.0)(_ * _)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft2(as, MyNil: MyList[A])((x, y) => MyCons(y, x))

  def appendAll[A](as: MyList[MyList[A]]): MyList[A] = foldRight(as, MyNil: MyList[A])((x, y) => x.append(y))

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = foldRight(as, MyNil: MyList[B])((x, y) => f(x).append(y))

  def filter2[A](as: MyList[A])(f: A => Boolean): MyList[A] = flatMap(as)(x => if (f(x)) MyList(x) else MyNil)

  def zipWith[A, B](a1: MyList[A], a2: MyList[A])(f: (A, A) => B): MyList[B] = (a1, a2) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (MyCons(a1, a1s), MyCons(a2, a2s)) => MyCons(f(a1, a2), zipWith(a1s, a2s)(f))
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    val supLen = length(sup)
    val subLen = length(sub)
    val ls = take(sup, subLen)
    if (supLen >= subLen)
      (ls == sub) || hasSubsequence(tail(sup), sub)
    else
      false
  }

  def fill[A](n: Int)(elem: => A): MyList[A] = {
    @tailrec
    def go(count: Int, list: MyList[A]): MyList[A] = if (count > 0) go(count - 1, MyCons(elem, list)) else list

    go(n, MyNil)
  }
}