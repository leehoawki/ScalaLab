package ch04

import ch03.{MyList, MyNil}
import org.scalatest._

class MyEitherSpec extends FlatSpec with Matchers {
  "MyEither" should "be correct" in {
    MyRight(1).map2(MyRight(2))(_ + _) shouldEqual MyRight(3)
    val x = MyLeft("1"): MyEither[String, Int]
    x.map2(MyRight(2))(_ + _) shouldEqual MyLeft("1")
    MyRight(2).map2(x)(_ + _) shouldEqual MyLeft("1")
  }
}