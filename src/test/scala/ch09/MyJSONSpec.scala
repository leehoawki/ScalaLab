package ch09

import ch04.MyEither
import org.scalatest.{FlatSpec, Matchers}

class MyJSONSpec extends FlatSpec with Matchers {
  "MyJson Parser" should "be correct" in {
    val jsonTxt =
      """
    {
      "Company name" : "Microsoft Corporation",
      "Ticker"  : "MSFT",
      "Active"  : true,
      "Price"   : 30.66,
      "Shares outstanding" : 8.38e9,
      "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
    }
    """

    val malformedJson1 =
      """
    {
    "Company name" ; "Microsoft Corporation"
    }
    """

    val malformedJson2 =
      """
    [
      [ "HPQ", "IBM",
      "YHOO", "DELL" ++
      "GOOG"
      ]
    ]
    """

    def printResult[E](e: MyEither[E, MyJSON]) = e.fold(println, println)

    //    val json: Parser[MyJSON] = MyJSON.jsonParser(P)
    //    printResult {      P.run(json)(jsonTxt)    }
    //    println("--")
    //    printResult {      P.run(json)(malformedJson1)    }
    //    println("--")
    //    printResult {      P.run(json)(malformedJson2)    }
  }
}
