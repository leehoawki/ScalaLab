package ch08

import ch03.{MyCons, MyList}
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

case class Prop(run: (Int, Int, MyRNG) => Result) {
  def &&(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: MyStream[Prop] = MyStream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val MyCons(x, xs) = props.map(p => Prop { (max, n, rng) => p.run(max, casesPerSize, rng) }).toList
      val prop: Prop = MyList.foldLeft(xs, x)(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zipWith(MyStream.from(0)) {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.take(n).find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: MyRNG): MyStream[A] =
    MyStream.unfold(rng)(rng => MySome(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[A](sample: MyState[MyRNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[MyList[A]] = size flatMap (n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(MyState(MyRNG.nextPositiveInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(MyState.unit(a))

  def boolean: Gen[Boolean] = Gen(MyState(MyRNG.nextPositiveInt).map(n => if (n % 2 == 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[MyList[A]] = Gen(MyState.sequence(MyList.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Int), g2: (Gen[A], Int)): Gen[A] = choose(0, g1._2 + g2._2).flatMap(n => if (n < g1._2) g1._1 else g2._1)

  def listOf[A](g: Gen[A]): SGen[MyList[A]] = SGen(n => Gen.listOfN(n, g))
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => g(n).flatMap(f(_).g(n)))
}