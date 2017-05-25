package ch08

import ch03.MyList
import ch06.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class MyGenSpec extends FlatSpec with Matchers {
  "Unit" should "be correct" in {
    Gen.unit(1).sample.run(SimpleRNG(0))._1 shouldEqual 1
  }

  "ListOfN" should "be correct" in {
    val result: MyList[Int] = Gen.listOfN(100, Gen.choose(0, 100)).sample.run(SimpleRNG(0))._1
    MyList.length(result) shouldEqual 100
    MyList.length(result.filter(_ >= 0)) shouldEqual 100
    MyList.length(result.filter(_ < 100)) shouldEqual 100
  }

  "ForAll" should "be correct" in {
    Prop.forAll(Gen.choose(0, 100))(_ >= 0).run(1, 100000, SimpleRNG(0)) shouldEqual Passed
    Prop.forAll(Gen.choose(0, 100))(_ < 0).run(1, 100000, SimpleRNG(0)).isFalsified shouldEqual true
  }

  "ForAll2" should "be correct" in {
    val smalls = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf(smalls)) { ns =>
      val max = 10
      !ns.exists(_ > max)
    }
    maxProp.run(100, 100, SimpleRNG(0)) shouldEqual Passed
  }
}