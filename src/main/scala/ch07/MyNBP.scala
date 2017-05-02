package ch07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

trait MyFuture[+A] {
  private[parallelism] def apply(k: A => Unit): Unit
}

// My Non Blocking Par
object MyNBP {
  type MyNBP[+A] = ExecutorService => MyFuture[A]

  def run[A](es: ExecutorService)(p: MyNBP[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): MyNBP[A] =
    es => new MyFuture[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => MyNBP[A]): MyNBP[A] =
    es => new MyFuture[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map2[A, B, C](p: MyNBP[A], p2: MyNBP[B])(f: (A, B) => C): MyNBP[C] = ???
}
