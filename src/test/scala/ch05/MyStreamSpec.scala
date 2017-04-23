package ch05

import ch03.{MyList, MyNil}
import ch04.{MyNone, MySome}
import org.scalatest.{FlatSpec, Matchers}

class MyStreamSpec extends FlatSpec with Matchers {
  "ToList" should "be correct" in {
    MyStream(1, 2, 3).toList shouldEqual MyList(1, 2, 3)
    MyStream().toList shouldEqual MyNil
    MyEmpty.toList shouldEqual MyNil
  }

  "Take" should "be correct" in {
    MyStream(1, 2, 3).take(-1).toList shouldEqual MyNil
    MyStream(1, 2, 3).take(0).toList shouldEqual MyNil
    MyStream(1, 2, 3).take(1).toList shouldEqual MyList(1)
    MyStream(1, 2, 3).take(2).toList shouldEqual MyList(1, 2)
    MyStream(1, 2, 3).take(3).toList shouldEqual MyList(1, 2, 3)
    MyStream(1, 2, 3).take(4).toList shouldEqual MyList(1, 2, 3)
    MyStream().take(0).toList shouldEqual MyNil
    MyStream().take(1).toList shouldEqual MyNil
  }

  "Drop" should "be correct" in {
    MyStream(1, 2, 3).drop(-1).toList shouldEqual MyList(1, 2, 3)
    MyStream(1, 2, 3).drop(0).toList shouldEqual MyList(1, 2, 3)
    MyStream(1, 2, 3).drop(1).toList shouldEqual MyList(2, 3)
    MyStream(1, 2, 3).drop(2).toList shouldEqual MyList(3)
    MyStream(1, 2, 3).drop(3).toList shouldEqual MyNil
    MyStream(1, 2, 3).drop(4).toList shouldEqual MyNil
    MyStream().drop(0).toList shouldEqual MyNil
    MyStream().drop(1).toList shouldEqual MyNil
  }

  "Takewhile" should "be correct" in {
    MyStream(1, 2, 3, 2, 1).takeWhile(_ < 0).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile(_ > 0).toList shouldEqual MyList(1, 2, 3, 2, 1)
    MyStream(1, 2, 3, 2, 1).takeWhile(_ > 2).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile(_ < 2).toList shouldEqual MyList(1)
    MyStream(1, 2, 3, 2, 1).takeWhile(_ < 3).toList shouldEqual MyList(1, 2)
    MyStream().takeWhile(_ => true).toList shouldEqual MyNil
    MyStream().takeWhile(_ => false).toList shouldEqual MyNil

    MyStream(1, 2, 3, 2, 1).takeWhile2(_ < 0).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile2(_ > 0).toList shouldEqual MyList(1, 2, 3, 2, 1)
    MyStream(1, 2, 3, 2, 1).takeWhile2(_ > 2).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile2(_ < 2).toList shouldEqual MyList(1)
    MyStream(1, 2, 3, 2, 1).takeWhile2(_ < 3).toList shouldEqual MyList(1, 2)

    MyStream().takeWhile2(_ => true).toList shouldEqual MyNil
    MyStream().takeWhile2(_ => false).toList shouldEqual MyNil

    MyStream(1, 2, 3, 2, 1).takeWhile3(_ < 0).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile3(_ > 0).toList shouldEqual MyList(1, 2, 3, 2, 1)
    MyStream(1, 2, 3, 2, 1).takeWhile3(_ > 2).toList shouldEqual MyNil
    MyStream(1, 2, 3, 2, 1).takeWhile3(_ < 2).toList shouldEqual MyList(1)
    MyStream(1, 2, 3, 2, 1).takeWhile3(_ < 3).toList shouldEqual MyList(1, 2)

    MyStream().takeWhile3(_ => true).toList shouldEqual MyNil
    MyStream().takeWhile3(_ => false).toList shouldEqual MyNil
  }

  "HeadOption" should "be correct" in {
    MyStream(1, 2, 3).headOption shouldEqual MySome(1)
    MyStream().headOption shouldEqual MyNone
  }

  "Append" should "be correct" in {
    MyEmpty.append(MyEmpty).toList shouldEqual MyNil
    MyEmpty.append(MyStream(1, 2, 3)).toList shouldEqual MyList(1, 2, 3)
    MyStream(1, 2, 3).append(MyEmpty).toList shouldEqual MyList(1, 2, 3)
    MyStream(1, 2, 3).append(MyStream(1, 2, 3)).toList shouldEqual MyList(1, 2, 3, 1, 2, 3)
  }
}