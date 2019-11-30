package fpinscala.datastructures

import Tree._

object TreeTest extends App {

  val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  val t2 = Branch(Branch(Leaf(1), Branch(Leaf(10), Leaf(0))), Branch(Leaf(3), Leaf(4)))

  println(size(t))
  println(maximum(t) + " " + maximum(t2))
  println(depth(t) + " " + depth(t2))

  println(map(t)(_ * 2))

  println(sizeF(t2))
  println(sizeF(Leaf(1)))

  println(maximumF(t2))

  println(depthF(t) + " " + depthF(t2))

  println(mapF(t2)(_ * 3))
}
