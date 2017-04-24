package ch06

import ch03.{MyCons, MyList, MyNil}
import ch05.MyStream

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    ((newSeed >>> 16).toInt, nextRNG)
  }
}

object RNG {
  def nextDouble(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nextPositiveInt(rng)
    if (i == Int.MaxValue) (0.0, rng2)
    else (i.toDouble / Int.MaxValue.toDouble, rng2)
  }

  def nextPositiveInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) (Int.MaxValue, rng2)
    else (i.abs, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = nextPositiveInt(rng)
    val (d, rng2) = nextDouble(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = nextDouble(rng)
    val (i, rng2) = nextPositiveInt(rng1)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = nextDouble(rng)
    val (d2, rng2) = nextDouble(rng1)
    val (d3, rng3) = nextDouble(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (MyList[Int], RNG) = {
    @tailrec
    def go(n: Int, rng: RNG, acc: MyList[Int]): (MyList[Int], RNG) = {
      if (n <= 0) (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(n - 1, rng2, MyCons(i, acc))
      }
    }

    go(count, rng, MyNil)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def int: Rand[Int] = rng => rng.nextInt

  def double: Rand[Double] = map(nextPositiveInt)(i => if (i == Int.MaxValue) 0 else i.toDouble / Int.MaxValue.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def sequence[A](fs: MyList[Rand[A]]): Rand[MyList[A]] = rng => {
    MyList.foldRight(fs, (MyNil: MyList[A], rng))((a, b) => {
      val (x, rngn) = a(b._2)
      (MyCons(x, b._1), rngn)
    })
  }

  def ints2(count: Int): Rand[MyList[Int]] = sequence(MyStream.constant(int).take(count).toList)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (x, rng2) = f(rng)
      g(x)(rng2)
    }
  }

  def nongNegtiveLessThan(n: Int): Rand[Int] = {
    flatMap(int) {
      a => {
        val m = a % n
        if (a + (n - 1) - m >= 0) unit(m)
        else nongNegtiveLessThan(n)
      }
    }
  }

  def mapByFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2ByFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a => mapByFlatMap(rb) { b => f(a, b) } }

  def map3ByFlatMap[A, B, C, D](ra: Rand[A], rb: Rand[B], rc: Rand[C])(f: (A, B, C) => D): Rand[D] = flatMap(ra) { a => flatMap(rb) { b => mapByFlatMap(rc) { c => f(a, b, c) } } }
}