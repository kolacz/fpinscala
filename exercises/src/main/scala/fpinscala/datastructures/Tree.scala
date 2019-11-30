package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(x, y) => maximum(x) max maximum(y)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(x, y) => (depth(x) + 1) max (depth(y) + 1)
    }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }

  def fold[A, B](t: Tree[A], f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l, f)(g), fold(r, f)(g))
    }

  def sizeF[A](t: Tree[A]): Int = fold(t, (_: A) => 1)((x, y) => 1 + x + y)

  def maximumF(t: Tree[Int]): Int = fold(t, (x: Int) => x)(_ max _)

  def depthF[A](t: Tree[A]): Int = fold(t, (_: A) => 0)((x, y) => (x + 1) max (y + 1))

  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t, ((x: B) => Leaf(x).asInstanceOf[Tree[B]]) compose f)((x, y) => Branch(x, y))

}
