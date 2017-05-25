package ch06

import ch03.{MyCons, MyList, MyNil}
import ch06.MyStack.{MyStack, pop, push}
import org.scalatest.{FlatSpec, Matchers}

class MyStackSpec extends FlatSpec with Matchers {
  "Push" should "be correct" in {
    def stackRun: MyState[MyStack, Unit] = {
      for {
        _ <- push(4)
        _ <- push(5)
        _ <- push(6)
      } yield ()
    }

    stackRun.run(MyList(1, 2, 3))._2 shouldEqual MyList(6, 5, 4, 1, 2, 3)
  }

  "Pop" should "be correct" in {
    def stackRun: MyState[MyStack, Int] = {
      for {
        a <- pop
        b <- pop
        c <- pop
        d <- pop
      } yield MyList.sum(MyList(a, b, c, d).map(_.getOrElse(0)))
    }

    stackRun.run(MyList(1, 2, 3))._1 shouldEqual 6
  }
}