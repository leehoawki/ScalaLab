package ch03

import org.scalatest._

class MyTreeSpec extends FlatSpec with Matchers {
  "TreeSize" should "be correct" in {
    MyTree.size(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 7
    MyTree.size(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 3
    MyTree.size(MyLeaf(1)) shouldEqual 1

    MyTree.size2(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 7
    MyTree.size2(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 3
    MyTree.size2(MyLeaf(1)) shouldEqual 1
  }

  "TreeMaximum" should "be correct" in {
    MyTree.maximum(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 4
    MyTree.maximum(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 2
    MyTree.maximum(MyLeaf(1)) shouldEqual 1

    MyTree.maximum2(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 4
    MyTree.maximum2(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 2
    MyTree.maximum2(MyLeaf(1)) shouldEqual 1
  }

  "TeeDepth" should "be correct" in {
    MyTree.depth(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 4
    MyTree.depth(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 2
    MyTree.depth(MyLeaf(1)) shouldEqual 1

    MyTree.depth2(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4))))) shouldEqual 4
    MyTree.depth2(MyBranch(MyLeaf(1), MyLeaf(2))) shouldEqual 2
    MyTree.depth2(MyLeaf(1)) shouldEqual 1
  }

  "TreeMap" should "be correct" in {
    MyTree.map(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4)))), (_ + 1): Int => Int) shouldEqual MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyBranch(MyLeaf(4), MyLeaf(5))))
    MyTree.map2(MyBranch(MyLeaf(1), MyBranch(MyLeaf(2), MyBranch(MyLeaf(3), MyLeaf(4)))), (_ * 2.0): Int => Double) shouldEqual MyBranch(MyLeaf(2), MyBranch(MyLeaf(4), MyBranch(MyLeaf(6), MyLeaf(8))))
  }
}
