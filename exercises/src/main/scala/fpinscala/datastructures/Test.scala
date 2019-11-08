package fpinscala.datastructures

import List._

object Test extends App {

  println(dropWhile(List(1,2,3,4,5,6,7), (x: Int) => x < 5))
  println(dropWhileCurried(List(1,2,3,4,5,6,7))(x => x < 5))
  println(lengthR(List(1,2,3,4,5,6,7,8,9,10,11)))

  def listOfLength(n: Int) = {
    @annotation.tailrec
    def go(n: Int, acc: List[Int]): List[Int] = {
      if (n > 0) go(n - 1, Cons(1, acc))
      else acc
    }
    go(n, Nil)
  }

  println(lengthR(listOfLength(1000))) 

  println(lengthL(listOfLength(1000000)))

  println(s"foldLeft sum: ${sumL(List(1,2,3,4,5,6,7,8,9,10))}")
  println(s"foldLeft product: ${productL(List(1,2,3,4,5,6,7,8,9,10))}")

  println(reverse(List(1,2,3,4,5,6,7)))
  println(reverseL(List(1,2,3,4,5,6,7)))

  //foldLeft: ((((((0 - 1) - 2) - 3) - 4) - 5) - 6) - 7 = -28
  //foldRight: 1 - (2 - (3 - (4 - (5 - (6 - (7 - 0)))))) = 4
  println(foldLeftR(List(1,2,3,4,5,6,7), 0)(_ - _))
  println(foldRightL(List(1,2,3,4,5,6,7), 0)(_ - _))

  println(appendR(List(1,2,3,4,5,6), List(7,8,9,10)))
  println(appendL(List(1,2,3), List(4,5,6,7,8,9,10)))

  println(flatten(List(List('a','b','c'), List('d','e','f','g'), List('h', 'i'), Nil, List('j'))))
}
