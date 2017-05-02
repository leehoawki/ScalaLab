package ch07

import java.util.concurrent.Executors

import ch03.MyList
import org.scalatest.{FlatSpec, Matchers}

class MyParSpec extends FlatSpec with Matchers {
  "Parmap" should "be correct" in {
    MyPar.run(Executors.newCachedThreadPool())(MyPar.parMap(MyList(1, 2, 3, 4))(_ + 1)).get() shouldEqual MyList(2, 3, 4, 5)
  }

  "ParFilter" should "be correct" in {
    MyPar.run(Executors.newCachedThreadPool())(MyPar.parFilter(MyList(1, 2, 3, 4))(_ > 1)).get() shouldEqual MyList(2, 3, 4)
  }
}