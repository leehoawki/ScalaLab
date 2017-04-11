package ch02

import org.scalatest._

class LabSpec extends FlatSpec with Matchers {
  "Fib1" should "be correct" in {
    Lab.fib(-1) shouldEqual 0
    Lab.fib(0) shouldEqual 0
    Lab.fib(1) shouldEqual 0
    Lab.fib(2) shouldEqual 1
    Lab.fib(3) shouldEqual 1
    Lab.fib(4) shouldEqual 2
    Lab.fib(5) shouldEqual 3
    Lab.fib(6) shouldEqual 5
  }

  "Fib2" should "be correct" in {
    Lab.fib2(-1) shouldEqual 0
    Lab.fib2(0) shouldEqual 0
    Lab.fib2(1) shouldEqual 0
    Lab.fib2(2) shouldEqual 1
    Lab.fib2(3) shouldEqual 1
    Lab.fib2(4) shouldEqual 2
    Lab.fib2(5) shouldEqual 3
    Lab.fib2(6) shouldEqual 5
  }

  "IsSorted" should "be correct" in {
    Lab.isSorted(Array(2, 1, 3, 5, 4), (x1: Int, x2: Int) => x1 > x2) shouldEqual false
    Lab.isSorted(Array(1, 2, 3, 4, 5), (x1: Int, x2: Int) => x1 > x2) shouldEqual false
    Lab.isSorted(Array(1, 2, 3, 4, 5), (x1: Int, x2: Int) => x1 < x2) shouldEqual true
  }
}
