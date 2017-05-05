package ch07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import ch03.MyList
import ch04._
import ch07.fpinscala.parallelism.Actor

trait MyFuture[+A] {
  def apply(k: A => Unit): Unit
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

  def unit[A](a: A): MyNBP[A] = es => (cb: A => Unit) => cb(a)

  def fork[A](a: => MyNBP[A]): MyNBP[A] = es => (cb: A => Unit) => eval(es)(a(es)(cb))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def map2[A, B, C](p: MyNBP[A], p2: MyNBP[B])(f: (A, B) => C): MyNBP[C] =
    es => (cb: C => Unit) => {
      var ar: MyOption[A] = MyNone
      var br: MyOption[B] = MyNone
      val combiner = Actor[MyEither[A, B]](es) {
        case MyLeft(a) => br match {
          case MyNone => ar = MySome(a)
          case MySome(b) => eval(es)(cb(f(a, b)))
        }
        case MyRight(b) => ar match {
          case MyNone => br = MySome(b)
          case MySome(a) => eval(es)(cb(f(a, b)))
        }
      }
      p(es)(a => combiner ! MyLeft(a))
      p2(es)(b => combiner ! MyRight(b))
    }

  def choice[A](cond: MyNBP[Boolean])(t: MyNBP[A], f: MyNBP[A]): MyNBP[A] = es => (cb: A => Unit) => cond(es) {
    b =>
      if (b) eval(es) {
        t(es)(cb)
      } else eval(es) {
        f(es)(cb)
      }
  }

  def choiceN[A](cond: MyNBP[Int])(choices: MyList[MyNBP[A]]): MyNBP[A] = es => (cb: A => Unit) => cond(es)(ind => eval(es)(choices.index(ind)(es)(cb)))
}
