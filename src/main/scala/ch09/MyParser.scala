package ch09

import ch03.{MyCons, MyList}
import ch04.MyEither

import scala.util.matching.Regex

trait MyParser[Parser[+ _]] {
  def run[A](p: Parser[A])(input: String): MyEither[ParseError, A]

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def listOfN[A](n: Int, p: Parser[A]): Parser[MyList[A]] = if (n <= 0) succeed(MyList()) else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[MyList[A]] = map2(p, many(p))(_ :: _) or succeed(MyList())

  def many1[A](p: Parser[A]): Parser[MyList[A]] = map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- p1; b <- p2} yield f(a, b)

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: => Parser[B]): Parser[B] = MyParser.this.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = MyParser.this.or(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = MyParser.this.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = MyParser.this.product(p, p2)

    def map[B](f: A => B): Parser[B] = MyParser.this.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = MyParser.this.flatMap(p)(f)
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  case class ParseError(stack: MyList[(Location, String)])

}