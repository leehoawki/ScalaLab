package ch09

import ch03.{MyCons, MyList}
import ch04.MyEither

trait MyParser[ParseError, Parser[+ _]] {
  def run[A](p: Parser[A])(input: String): MyEither[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[MyList[A]]

  def many[A](p: Parser[A]): Parser[MyList[A]]

  def many1[A](p: Parser[A]): Parser[MyList[A]] = map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- p1; b <- p2} yield f(a, b)

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = MyParser.this.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = MyParser.this.or(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = MyParser.this.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = MyParser.this.product(p, p2)

    def map[B](f: A => B): Parser[B] = MyParser.this.map(p)(f)
  }
}