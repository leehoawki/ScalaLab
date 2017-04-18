package ch04

import ch03.MyList

object Lab {
  def avg(xs: MyList[Double]): MyOption[Double] = {
    val s = MyList.foldLeft(xs, 0.0)(_ + _)
    val l = MyList.foldLeft(xs, 0)((x, y) => x + 1)
    if (l == 0) MyNone else MySome(s / l)
  }

  def variance(xs: MyList[Double]) = avg(xs).flatMap(m => MyList.foldLeft(xs, MySome(0.0): MyOption[Double])((x, y) => x.flatMap(z => MySome(z + Math.pow(y - m, 2)))))
}