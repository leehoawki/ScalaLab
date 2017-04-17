package ch04

import ch03.{MyCons, MyList, MyNil}

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]

  def flatMap[B](f: A => MyOption[B]): MyOption[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B]

  def filter(f: A => Boolean): MyOption[A]
}

case class MySome[+A](get: A) extends MyOption[A] {
  override def map[B](f: (A) => B): MyOption[B] = MySome(f(get))

  override def flatMap[B](f: (A) => MyOption[B]): MyOption[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this

  override def filter(f: (A) => Boolean): MyOption[A] = if (f(get)) this else MyNone
}

case object MyNone extends MyOption[Nothing] {
  override def map[B](f: (Nothing) => B): MyOption[B] = MyNone

  override def flatMap[B](f: (Nothing) => MyOption[B]): MyOption[B] = MyNone

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => MyOption[B]): MyOption[B] = ob

  override def filter(f: (Nothing) => Boolean): MyOption[Nothing] = MyNone
}

case object MyOption {
  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))

  def sequence[A](a: MyList[MyOption[A]]): MyOption[MyList[A]] = {
    def f[A](a: A, b: MyList[A]): MyList[A] = MyCons(a, b)

    def g[A](a: MyOption[A], b: MyOption[MyList[A]]): MyOption[MyList[A]] = map2(a, b)(f)

    MyList.foldRight(a, MySome(MyNil): MyOption[MyList[A]])(g)
  }

  def traverse[A, B](a: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] = {
    def g[A](a: A, b: MyOption[MyList[B]]): MyOption[MyList[B]] = map2(f(a), b)((x, y) => MyCons(x, y))

    MyList.foldRight(a, MySome(MyNil): MyOption[MyList[B]])(g)
  }
}
