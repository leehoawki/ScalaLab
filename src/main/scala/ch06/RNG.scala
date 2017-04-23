package ch06

import ch03.MyList

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

  def ints(count: Int)(rng: RNG): (MyList[Int], RNG) = ???
}