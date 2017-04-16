package ch03

import org.scalatest._

class MyListSpec extends FlatSpec with Matchers {
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
    MyList.tail(MyList(1, 2, 3)) shouldEqual MyList(2, 3)
    MyList.tail(MyList(1)) shouldEqual MyList()
    MyList.tail(MyList(1)) shouldEqual MyNil
    MyList.tail(MyNil) shouldEqual MyNil
  }

  "SetHead" should "be correct" in {
    MyList.setHead(0, MyList(1, 2, 3)) shouldEqual MyList(0, 1, 2, 3)
    MyList.setHead(0, MyNil) shouldEqual MyList(0)
  }

  "Drop" should "be correct" in {
    MyList.drop(MyList(1, 2, 3), -1) shouldEqual MyList(1, 2, 3)
    MyList.drop(MyList(1, 2, 3), 0) shouldEqual MyList(1, 2, 3)
    MyList.drop(MyList(1, 2, 3), 1) shouldEqual MyList(2, 3)
    MyList.drop(MyList(1, 2, 3), 2) shouldEqual MyList(3)
    MyList.drop(MyList(1, 2, 3), 3) shouldEqual MyNil
    MyList.drop(MyList(1, 2, 3), 4) shouldEqual MyNil
    MyList.drop(MyNil, -1) shouldEqual MyNil
    MyList.drop(MyNil, 0) shouldEqual MyNil
    MyList.drop(MyNil, 1) shouldEqual MyNil
  }

  "DropWhile" should "be correct" in {
    MyList.dropWhile(MyList(1, 2, 3), (x: Int) => x > 0) shouldEqual MyNil
    MyList.dropWhile(MyList(1, 2, 3), (x: Int) => x < 0) shouldEqual MyList(1, 2, 3)
    MyList.dropWhile(MyList(1, 2, 3), (x: Int) => x < 2) shouldEqual MyList(2, 3)
    MyList.dropWhile(MyNil, (x: Int) => x < 0) shouldEqual MyNil

    MyList.dropWhile2(MyList(1, 2, 3))(x => x > 0) shouldEqual MyNil
  }

  "Init" should "be correct" in {
    MyList.init(MyList(1, 2, 3)) shouldEqual MyList(1, 2)
    MyList.init(MyList(1)) shouldEqual MyNil
    MyList.init(MyNil) shouldEqual MyNil
  }

  "Length" should "be correct" in {
    MyList.length(MyList(1, 2, 3)) shouldEqual 3
    MyList.length(MyList(1)) shouldEqual 1
    MyList.length(MyNil) shouldEqual 0

    MyList.length2(MyList(1, 2, 3)) shouldEqual 3
    MyList.length2(MyList(1)) shouldEqual 1
    MyList.length2(MyNil) shouldEqual 0
  }

  "Reverse" should "be correct" in {
    MyList.reverse(MyList(1, 2, 3)) shouldEqual MyList(3, 2, 1)
    MyList.reverse(MyList(1)) shouldEqual MyList(1)
    MyList.reverse(MyNil) shouldEqual MyNil
  }

  "Append" should "be correct" in {
    MyList.append(MyList(1, 2, 3), MyList(4, 5, 6)) shouldEqual MyList(1, 2, 3, 4, 5, 6)
    MyList.append(MyNil, MyList(4, 5, 6)) shouldEqual MyList(4, 5, 6)
    MyList.append(MyList(4, 5, 6), MyNil) shouldEqual MyList(4, 5, 6)
    MyList.append(MyNil, MyNil) shouldEqual MyNil

    MyList.appendAll(MyList(MyList(1, 2, 3), MyList(4, 5, 6))) shouldEqual MyList(1, 2, 3, 4, 5, 6)
    MyList.appendAll(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8, 9))) shouldEqual MyList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    MyList.appendAll(MyList(MyNil, MyList(4, 5, 6), MyList(7, 8, 9))) shouldEqual MyList(4, 5, 6, 7, 8, 9)
    MyList.appendAll(MyList(MyNil, MyNil)) shouldEqual MyNil
    MyList.appendAll(MyNil) shouldEqual MyNil
  }

  "FlatMap" should "be correct" in {
    MyList.flatMap(MyList(1, 2, 3))(x => MyList(x)) shouldEqual MyList(1, 2, 3)
    MyList.flatMap(MyList(1, 2, 3))(x => MyList(x, x)) shouldEqual MyList(1, 1, 2, 2, 3, 3)
  }

  "Filter" should "be correct" in {
    MyList.filter(MyList(1, 2, 3))(_ > 0) shouldEqual MyList(1, 2, 3)
    MyList.filter(MyList(1, 2, 3))(_ < 0) shouldEqual MyNil
    MyList.filter(MyNil: MyList[Int])(_ < 0) shouldEqual MyNil

    MyList.filter2(MyList(1, 2, 3))(_ > 0) shouldEqual MyList(1, 2, 3)
    MyList.filter2(MyList(1, 2, 3))(_ < 0) shouldEqual MyNil
    MyList.filter2(MyNil: MyList[Int])(_ < 0) shouldEqual MyNil
  }

  "Zipwith" should "be correct" in {
    MyList.zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ + _) shouldEqual MyList(5, 7, 9)
    MyList.zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ * _) shouldEqual MyList(4, 10, 18)
    MyList.zipWith(MyNil, MyList(4, 5, 6))(_ * _) shouldEqual MyNil
    MyList.zipWith(MyList(4, 5, 6), MyNil)(_ * _) shouldEqual MyNil
  }

  "HasSubsequence" should "be correct" in {
    MyList.hasSubsequence(MyList(4, 1, 1, 2, 3, 7, 8, 9), MyList(1, 2, 3)) shouldEqual true
    MyList.hasSubsequence(MyList(4, 1, 1, 2, 3, 7, 8, 9), MyList(1, 2, 3, 4)) shouldEqual false
    MyList.hasSubsequence(MyNil, MyList(1, 2, 3)) shouldEqual false
    MyList.hasSubsequence(MyNil, MyNil) shouldEqual true
    MyList.hasSubsequence(MyList(1), MyList(1)) shouldEqual true
    MyList.hasSubsequence(MyList(1, 2, 3), MyNil) shouldEqual true
    MyList.hasSubsequence(MyList(4, 1, 1, 2, 3, 7, 8, 9), MyList(1, 2)) shouldEqual true
  }
}
