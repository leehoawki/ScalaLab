package ch07

import java.util.concurrent.Executors

import ch03.MyList
import org.scalatest.{FlatSpec, Matchers}

class MyNBPSpec extends FlatSpec with Matchers {
  "Parmap" should "be correct" in {
    MyNBP.run(Executors.newCachedThreadPool())(MyNBP.parMap(MyList(1, 2, 3, 4))(_ + 1)) shouldEqual MyList(2, 3, 4, 5)
  }
}