package ch03

sealed trait MyTree[+A]

case class MyLeaf[A](value: A) extends MyTree[A]

case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def size[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: MyTree[Int]): Int = t match {
    case MyLeaf(v) => v
    case MyBranch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: MyTree[A]): Int = t match {
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: MyTree[A], f: A => B): MyTree[B] = t match {
    case MyLeaf(v) => MyLeaf(f(v))
    case MyBranch(l, r) => MyBranch(map(l, f), map(r, f))
  }

  def fold[A, B](t: MyTree[A], fl: (A) => B, fb: (B, B) => B): B = t match {
    case MyLeaf(v) => fl(v)
    case MyBranch(l, r) => fb(fold(l, fl, fb), fold(r, fl, fb))
  }

  def size2[A](t: MyTree[A]): Int = fold(t, (v: A) => 1, (i: Int, j: Int) => i + j + 1)

  def maximum2(t: MyTree[Int]): Int = fold(t, (v: Int) => v, (i: Int, j: Int) => if (i >= j) i else j)

  def depth2[A](t: MyTree[A]): Int = fold(t, (v: A) => 1, (i: Int, j: Int) => 1 + (i max j))

  def map2[A, B](t: MyTree[A], f: A => B): MyTree[B] = fold(t, (v: A) => MyLeaf(f(v)), (t1: MyTree[B], t2: MyTree[B]) => MyBranch(t1, t2))
}