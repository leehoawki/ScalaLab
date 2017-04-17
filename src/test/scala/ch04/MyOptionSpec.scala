package ch04


import ch03.{MyList, MyNil}
import org.scalatest._

class MyOptionSpec extends FlatSpec with Matchers {
  "Variance" should "be correct" in {
    Lab.variance(MyList(1, 2, 3)) shouldEqual MySome(2.0)
    Lab.variance(MyList()) shouldEqual MyNone
  }

  "Sequence" should "be correct" in {
    MyOption.sequence(MyNil) shouldEqual MySome(MyNil)
    MyOption.sequence(MyList(MySome(1))) shouldEqual MySome(MyList(1))
    MyOption.sequence(MyList(MySome(1), MySome(2), MySome(3))) shouldEqual MySome(MyList(1, 2, 3))
    MyOption.sequence(MyList(MySome(1), MyNone, MySome(3))) shouldEqual MyNone
    MyOption.sequence(MyList(MyNone)) shouldEqual MyNone
  }
}