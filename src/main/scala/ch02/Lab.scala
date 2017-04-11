package ch02

object Lab {
  def fib(n: Int): Int =
    if (n < 0) 0
    else n match {
      case 1 => 0
      case 2 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, c: Int): Int =
      if (c <= 1) b
      else go(a + b, a, c - 1)

    go(1, 0, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1)
        true
      else if (ordered(as(n), as(n + 1))) go(n + 1)
      else false

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}