package ch10

import ch03.{MyList, MyNil}
import ch04.{MyNone, MyOption}
import ch07.MyPar.MyPar
import ch08.{Gen, Prop}

trait MyMonoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object MyMonoid {
  val stringMoniod = new MyMonoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val intAddition = new MyMonoid[Int] {
    def op(x: Int, y: Int) = x + y

    val zero = 0
  }

  val intMultiplication = new MyMonoid[Int] {
    def op(x: Int, y: Int) = x * y

    val zero = 1
  }

  val booleanOr = new MyMonoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y

    val zero = false
  }

  val booleanAnd = new MyMonoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y

    val zero = true
  }

  def listMoniod[A] = new MyMonoid[MyList[A]] {
    override def op(a1: MyList[A], a2: MyList[A]): MyList[A] = a1 ++ a2

    override def zero = MyNil
  }

  def optionMonoid[A] = new MyMonoid[MyOption[A]] {
    override def op(a1: MyOption[A], a2: MyOption[A]): MyOption[A] = a1 orElse a2

    override def zero: MyOption[A] = MyNone
  }

  def endoMonoid[A]: MyMonoid[A => A] = new MyMonoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    override def zero: (A) => A = _
  }

  def monoidLaws[A](m: MyMonoid[A], gen: Gen[A]): Prop = ???

  def foldMap[A, B](as: MyList[A], m: MyMonoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = ???

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = ???

  def foldMapV[A, B](as: IndexedSeq[A], m: MyMonoid[B])(f: A => B): B = ???

  def par[A](m: MyMonoid[A]): MyMonoid[MyPar[A]] = ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: MyMonoid[B])(f: A => B): MyPar[B] = ???

  def ordered(ints: IndexedSeq[Int]): Boolean = ???
}
