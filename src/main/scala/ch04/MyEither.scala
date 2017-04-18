package ch04

import ch03.{MyCons, MyList, MyNil}
import ch04.MyOption.map2

sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B]

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B]

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B]

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C]
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing] {
  override def map[B](f: (Nothing) => B): MyEither[E, B] = this

  override def flatMap[EE >: E, B](f: (Nothing) => MyEither[EE, B]): MyEither[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => MyEither[EE, B]): MyEither[EE, B] = b

  override def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (Nothing, B) => C): MyEither[EE, C] = this
}

case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
  override def map[B](f: (A) => B): MyEither[Nothing, B] = MyRight(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => MyEither[EE, B]): MyEither[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = b map (bb => f(value, bb))
}

case object MyEither {
  def sequence[E, A](as: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] = traverse(as)(x => x)

  def map2[E, A, B, C](a: MyEither[E, A], b: MyEither[E, B])(f: (A, B) => C): MyEither[E, C] = a.map2(b)(f)

  def traverse[E, A, B](as: MyList[A])(f: A => MyEither[E, B]): MyEither[E, MyList[B]] = {
    def g(a: A, b: MyEither[E, MyList[B]]): MyEither[E, MyList[B]] = map2(f(a), b)((x, y) => MyCons(x, y))

    MyList.foldRight(as, MyRight(MyNil): MyEither[E, MyList[B]])(g)
  }
}