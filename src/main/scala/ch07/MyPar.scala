package ch07

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import ch03.{MyCons, MyList, MyNil}

object MyPar {
  type MyPar[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: MyPar[A]): Future[A] = a(es)

  def fork[A](a: => MyPar[A]): MyPar[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def unit[A](a: A): MyPar[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): MyPar[A] = fork(unit(a))

  def map2[A, B, C](a: MyPar[A], b: MyPar[B])(f: (A, B) => C): MyPar[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def asyncF[A, B](f: A => B): A => MyPar[B] = a => lazyUnit(f(a))

  def map[A, B](pa: MyPar[A])(f: A => B): MyPar[B] = map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](as: MyList[MyPar[A]]): MyPar[MyList[A]] = as match {
    case MyNil => unit(MyNil)
    case MyCons(h, t) => map2(h, sequence(t))(MyCons.apply)
  }

  def parMap[A, B](as: MyList[A])(f: A => B): MyPar[MyList[B]] = sequence(as.map(asyncF(f)))

  def parFilter[A](as: MyList[A])(f: A => Boolean): MyPar[MyList[A]] = {
    val pars: MyList[MyPar[MyList[A]]] = as map (asyncF((a: A) => if (f(a)) MyList(a) else MyNil))
    map(sequence(pars))(MyList.appendAll)
  }
}

case class UnitFuture[A](get: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get
}