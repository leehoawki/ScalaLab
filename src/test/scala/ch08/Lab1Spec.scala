package ch08

import ch03.MyList
import ch06.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class Lab1Spec extends FlatSpec with Matchers {
  "Unit" should "be correct" in {
    Gen1.unit(1).sample.run(SimpleRNG(0))._1 shouldEqual 1
  }

  "ListOfN" should "be correct" in {
    val result: MyList[Int] = Gen1.listOfN(100, Gen1.choose(0, 100)).sample.run(SimpleRNG(0))._1
    MyList.length(result) shouldEqual 100
    MyList.length(result.filter(_ >= 0)) shouldEqual 100
    MyList.length(result.filter(_ < 100)) shouldEqual 100
  }

  "ForAll" should "be correct" in {
    Prop1.forAll(Gen1.choose(0, 100))(_ >= 0).run(100000, SimpleRNG(0)) shouldEqual Passed
    Prop1.forAll(Gen1.choose(0, 100))(_ < 0).run(100000, SimpleRNG(0)).isFalsified shouldEqual true
  }
}