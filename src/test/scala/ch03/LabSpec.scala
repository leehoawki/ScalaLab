package ch03

import org.scalatest._

class LabSpec extends FlatSpec with Matchers {
  "xList" should "be correct" in {
    xList.sum(xNil) shouldEqual 0
    xList.sum(xList()) shouldEqual 0
    xList.sum(xList(1,2,3,4)) shouldEqual 10

    xList.product(xNil) shouldEqual 1
    xList.product(xList()) shouldEqual 1
    xList.product(xList(1,2,3,4)) shouldEqual 24
  }

  "Tail" should "be correct" in {
    Lab.tail(xList(1, 2, 3)) shouldEqual xList(2, 3)
    Lab.tail(xList(1)) shouldEqual xList()
    Lab.tail(xList(1)) shouldEqual xNil
    Lab.tail(xNil) shouldEqual xNil
  }

  "SetHead" should "be correct" in {
    Lab.setHead(0, xList(1, 2, 3)) shouldEqual xList(0, 1, 2, 3)
    Lab.setHead(0, xList()) shouldEqual xList(0)
    Lab.setHead(0, xNil) shouldEqual xList(0)
  }

}
