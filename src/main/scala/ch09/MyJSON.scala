package ch09

import ch03.MyList

trait MyJSON

case object MyJNull extends MyJSON

case class MyJNumber(get: Double) extends MyJSON

case class MyJString(get: String) extends MyJSON

case class MyJBool(get: Boolean) extends MyJSON

case class MyJArray(get: MyList[MyJSON]) extends MyJSON

case class MyJObject(get: Map[String, MyJSON]) extends MyJSON

object MyJSON {
  def jsonParser[Parser[+ _]](P: MyParser[Parser]): Parser[MyJSON] = ???
}
