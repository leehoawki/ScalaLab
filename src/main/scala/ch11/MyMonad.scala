package ch11

import ch03.{MyCons, MyList, MyNil}

trait MyMonad[F[_]] extends MyFunctor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => flatMap(mb)(b => unit(f(a, b))))

  def sequence[A](lma: MyList[F[A]]): F[MyList[A]] = lma.foldRight(unit(MyList[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: MyList[A])(f: A => F[B]): F[MyList[B]] = la.foldRight(unit(MyList[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[MyList[A]] = if (n <= 0) unit(MyList[A]()) else map2(ma, replicateM(n - 1, ma))(_ :: _)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: MyList[A])(f: A => F[Boolean]): F[MyList[A]] = ms match {
    case MyNil => unit(MyNil)
    case MyCons(h, t) => flatMap(f(h))(b =>
      if (!b) filterM(t)(f)
      else map(filterM(t)(f))(h :: _))
  }
}
