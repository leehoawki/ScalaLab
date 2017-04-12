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
    Lab.setHead(0, MyList()) shouldEqual MyList(0)
    Lab.setHead(0, MyNil) shouldEqual MyList(0)
  }

}
