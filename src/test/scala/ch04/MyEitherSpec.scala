package ch04

import org.scalatest._

class MyEitherSpec extends FlatSpec with Matchers {
  "MyEither" should "be correct" in {
    MyRight(1).map2(MyRight(2))(_ + _) shouldEqual MyRight(3)
    val x = MyLeft("1"): MyEither[String, Int]
    x.map2(MyRight(2))(_ + _) shouldEqual MyLeft("1")
    MyRight(2).map2(x)(_ + _) shouldEqual MyLeft("1")
  }

  "Fold" should "be correct" in {
    val m1: MyEither[Int, Int] = MyLeft(1)
    val m2: MyEither[Int, Int] = MyRight(1)
    m1.fold(1.+, 1.+) shouldEqual 2
    m2.fold(1.+, 1.+) shouldEqual 2
  }
}