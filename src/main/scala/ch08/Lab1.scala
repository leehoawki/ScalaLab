package ch08

import ch03.MyList
import ch04.MySome
import ch05.MyStream
import ch06.{MyRNG, MyState}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified: Boolean = false
}

case class Falsified(failure: String, successes: Int) extends Result {
  def isFalsified: Boolean = true
}

case class Prop1(run: (Int, MyRNG) => Result) {
  def &&(p: Prop1) = Prop1 {
    (n, rng) =>
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case x => x
      }
  }

  def ||(p: Prop1) = Prop1 {
    (n, rng) =>
      run(n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(n, rng)
        case x => x
      }
  }

  def tag(msg: String) = Prop1 {
    (n, rng) =>
      run(n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop1 {
  def forAll[A](as: Gen1[A])(f: A => Boolean): Prop1 = Prop1 {
    (n, rng) =>
      randomStream(as)(rng).zipWith(MyStream.from(0)) {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.take(n).find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen1[A])(rng: MyRNG): MyStream[A] =
    MyStream.unfold(rng)(rng => MySome(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen1[A](sample: MyState[MyRNG, A]) {
  def flatMap[B](f: A => Gen1[B]): Gen1[B] = Gen1(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen1[Int]): Gen1[MyList[A]] = size flatMap (n => Gen1.listOfN(n, this))
}

object Gen1 {
  def choose(start: Int, stopExclusive: Int): Gen1[Int] = Gen1(MyState(MyRNG.nextPositiveInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen1[A] = Gen1(MyState.unit(a))

  def boolean: Gen1[Boolean] = Gen1(MyState(MyRNG.nextPositiveInt).map(n => if (n % 2 == 0) true else false))

  def listOfN[A](n: Int, g: Gen1[A]): Gen1[MyList[A]] = Gen1(MyState.sequence(MyList.fill(n)(g.sample)))

  def union[A](g1: Gen1[A], g2: Gen1[A]): Gen1[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen1[A], Int), g2: (Gen1[A], Int)): Gen1[A] = choose(0, g1._2 + g2._2).flatMap(n => if (n < g1._2) g1._1 else g2._1)
}
