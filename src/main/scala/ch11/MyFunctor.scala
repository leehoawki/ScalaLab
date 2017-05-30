package ch11

import ch03.MyList
import ch04.{MyEither, MyLeft, MyRight}

trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: MyEither[F[A], F[B]]): F[MyEither[A, B]] = e match {
    case MyLeft(fa) => map(fa)(MyLeft(_))
    case MyRight(fb) => map(fb)(MyRight(_))
  }
}

object MyFunctor {
  val listFunctor = new MyFunctor[MyList] {
    override def map[A, B](as: MyList[A])(f: (A) => B): MyList[B] = as map f
  }
}