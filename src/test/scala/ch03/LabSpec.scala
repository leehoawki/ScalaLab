package ch03

import org.scalatest._

class LabSpec extends FlatSpec with Matchers {
  "xList" should "be correct" in {
    MyList.sum(MyNil) shouldEqual 0
    MyList.sum(MyList()) shouldEqual 0
    MyList.sum(MyList(1)) shouldEqual 1
    MyList.sum(MyList(1, 2, 3, 4)) shouldEqual 10

    MyList.product(MyNil) shouldEqual 1
    MyList.product(MyList()) shouldEqual 1
    MyList.product(MyList(1)) shouldEqual 1
    MyList.product(MyList(1, 2, 3, 4)) shouldEqual 24
  }

  "Tail" should "be correct" in {
    Lab.tail(MyList(1, 2, 3)) shouldEqual MyList(2, 3)
    Lab.tail(MyList(1)) shouldEqual MyList()
    Lab.tail(MyList(1)) shouldEqual MyNil
    Lab.tail(MyNil) shouldEqual MyNil
  }

  "SetHead" should "be correct" in {
    Lab.setHead(0, MyList(1, 2, 3)) shouldEqual MyList(0, 1, 2, 3)
    Lab.setHead(0, MyNil) shouldEqual MyList(0)
  }

  "Drop" should "be correct" in {
    Lab.drop(MyList(1, 2, 3), -1) shouldEqual MyList(1, 2, 3)
    Lab.drop(MyList(1, 2, 3), 0) shouldEqual MyList(1, 2, 3)
    Lab.drop(MyList(1, 2, 3), 1) shouldEqual MyList(2, 3)
    Lab.drop(MyList(1, 2, 3), 2) shouldEqual MyList(3)
    Lab.drop(MyList(1, 2, 3), 3) shouldEqual MyNil
    Lab.drop(MyList(1, 2, 3), 4) shouldEqual MyNil
    Lab.drop(MyNil, -1) shouldEqual MyNil
    Lab.drop(MyNil, 0) shouldEqual MyNil
    Lab.drop(MyNil, 1) shouldEqual MyNil
  }

  "DropWhile" should "be correct" in {
    Lab.dropWhile(MyList(1, 2, 3), (x: Int) => x > 0) shouldEqual MyNil
    Lab.dropWhile(MyList(1, 2, 3), (x: Int) => x < 0) shouldEqual MyList(1, 2, 3)
    Lab.dropWhile(MyList(1, 2, 3), (x: Int) => x < 2) shouldEqual MyList(2, 3)
    Lab.dropWhile(MyNil, (x: Int) => x < 0) shouldEqual MyNil

    Lab.dropWhile2(MyList(1, 2, 3))(x => x > 0) shouldEqual MyNil
  }

  "Init" should "be correct" in {
    Lab.init(MyList(1, 2, 3)) shouldEqual MyList(1, 2)
    Lab.init(MyList(1)) shouldEqual MyNil
    Lab.init(MyNil) shouldEqual MyNil
  }

  "Length" should "be correct" in {
    Lab.length(MyList(1, 2, 3)) shouldEqual 3
    Lab.length(MyList(1)) shouldEqual 1
    Lab.length(MyNil) shouldEqual 0

    Lab.length2(MyList(1, 2, 3)) shouldEqual 3
    Lab.length2(MyList(1)) shouldEqual 1
    Lab.length2(MyNil) shouldEqual 0
  }

  "Reverse" should "be correct" in {
    Lab.reverse(MyList(1, 2, 3)) shouldEqual MyList(3, 2, 1)
    Lab.reverse(MyList(1)) shouldEqual MyList(1)
    Lab.reverse(MyNil) shouldEqual MyNil
  }

  "Append" should "be correct" in {
    Lab.append(MyList(1, 2, 3), MyList(4, 5, 6)) shouldEqual MyList(1, 2, 3, 4, 5, 6)
    Lab.append(MyNil, MyList(4, 5, 6)) shouldEqual MyList(4, 5, 6)
    Lab.append(MyList(4, 5, 6), MyNil) shouldEqual MyList(4, 5, 6)
    Lab.append(MyNil, MyNil) shouldEqual MyNil

    Lab.appendAll(MyList(MyList(1, 2, 3), MyList(4, 5, 6))) shouldEqual MyList(1, 2, 3, 4, 5, 6)
    Lab.appendAll(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8, 9))) shouldEqual MyList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    Lab.appendAll(MyList(MyNil, MyList(4, 5, 6), MyList(7, 8, 9))) shouldEqual MyList(4, 5, 6, 7, 8, 9)
    Lab.appendAll(MyList(MyNil, MyNil)) shouldEqual MyNil
    Lab.appendAll(MyNil) shouldEqual MyNil
  }
}
