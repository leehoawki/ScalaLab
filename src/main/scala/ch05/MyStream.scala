package ch05

import ch03.{MyCons, MyList, MyNil}
import ch04.{MyNone, MyOption, MySome}
import ch05.MyStream.unfold

sealed trait MyStream[+A] {

  def take(n: Int): MyStream[A]

  def drop(n: Int): MyStream[A]

  def takeWhile(p: A => Boolean): MyStream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case MyScons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def toList: MyList[A] = foldRight(MyNil: MyList[A])((a, b) => MyCons(a, b))

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean) = foldRight(MyEmpty: MyStream[A])((a, b) => if (p(a)) MyStream.scons(a, b) else MyEmpty)

  def headOption: MyOption[A] = foldRight(MyNone: MyOption[A])((a, b) => MySome(a))

  def map[B](f: A => B): MyStream[B] = foldRight(MyEmpty: MyStream[B])((a, b) => MyStream.scons(f(a), b))

  def filter(p: A => Boolean): MyStream[A] = foldRight(MyEmpty: MyStream[A])((a, b) => if (p(a)) MyStream.scons(a, b) else b)

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = foldRight(MyEmpty: MyStream[B])((a, b) => f(a).append(b))

  def append[B >: A](b: => MyStream[B]): MyStream[B] = foldRight(b)((x, y) => MyStream.scons(x, y))

  def map2[B](f: A => B): MyStream[B] = unfold(this) {
    case MyScons(h, t) => MySome(f(h()), t())
    case MyEmpty => MyNone
  }

  def take2(n: Int): MyStream[A] = unfold((this, n)) {
    case (MyEmpty, _) => MyNone
    case (_, n) if n <= 0 => MyNone
    case (MyScons(h, t), n) if n > 0 => MySome((h()), (t(), n - 1))
  }

  def takeWhile3(p: A => Boolean) = unfold(this) {
    case MyEmpty => MyNone
    case MyScons(h, t) => if (p(h())) MySome((h()), t()) else MyNone
  }

  def zipWith[B >: A, C](s2: MyStream[B])(f: (A, B) => C): MyStream[C] = unfold((this, s2)) {
    case (MyEmpty, _) => MyNone
    case (_, MyEmpty) => MyNone
    case (MyScons(h1, t1), MyScons(h2, t2)) => MySome((f(h1(), h2()), (t1(), t2())))
  }

  def zipAll[B](s2: MyStream[B]): MyStream[(MyOption[A], MyOption[B])] = unfold((this, s2)) {
    case (MyEmpty, MyEmpty) => MyNone
    case (MyScons(h1, t1), MyEmpty) => MySome((MySome(h1()), MyNone), (t1(), MyEmpty))
    case (MyEmpty, MyScons(h2, t2)) => MySome((MyNone, MySome(h2())), (MyEmpty, t2()))
    case (MyScons(h1, t1), MyScons(h2, t2)) => MySome((MySome(h1()), MySome(h2())), (t1(), t2()))
  }

  def startsWith[B >: A](s: MyStream[B]): Boolean = zipAll(s).foldRight(true)((a, b) => {
    a match {
      case (MySome(_), MyNone) => true
      case (MySome(x1), MySome(x2)) => (x1 == x2) && b
      case _ => false
    }
  })

  def tails: MyStream[MyStream[A]] = unfold(this) {
    case MyEmpty => MyNone
    case a@MyScons(h, t) => MySome(a, t())
  }

  def hasSubsequence[B >: A](s: MyStream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] = unfold(this) {
    case MyEmpty => MyNone
    case a@MyScons(h, t) => MySome((a.foldRight(z)(f), t()))
  }
}

case object MyEmpty extends MyStream[Nothing] {
  override def take(n: Int): MyStream[Nothing] = this

  override def drop(n: Int): MyStream[Nothing] = this

  override def takeWhile(p: (Nothing) => Boolean): MyStream[Nothing] = this
}

case class MyScons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A] {
  override def take(n: Int): MyStream[A] = if (n > 0) MyScons(h, () => t().take(n - 1)) else MyEmpty

  override def drop(n: Int): MyStream[A] = if (n <= 0) MyScons(h, () => t()) else t().drop(n - 1)

  override def takeWhile(p: (A) => Boolean): MyStream[A] = if (p(h())) MyScons(h, () => t().takeWhile(p)) else MyEmpty
}

object MyStream {
  def scons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    MyScons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] = if (as.isEmpty) empty else scons(as.head, apply(as.tail: _*))

  def constant[A](a: A): MyStream[A] = scons(a, constant(a))

  def from(n: Int): MyStream[Int] = scons(n, from(n + 1))

  def fibs: MyStream[Int] = {
    def go(prev: Int, cur: Int): MyStream[Int] = {
      scons(prev, go(cur, prev + cur))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => MyOption[(A, S)]): MyStream[A] = f(z) match {
    case MyNone => empty
    case MySome((a, s)) => scons(a, unfold(s)(f))
  }

  def constant2[A](a: A): MyStream[A] = unfold(a) { case x => MySome(x, x) }

  def from2(n: Int): MyStream[Int] = unfold(n) { case a => MySome(a, a + 1) }

  def fibs2: MyStream[Int] = unfold((0, 1)) { case (a1, a2) => MySome((a1, (a2, a1 + a2))) }
}
