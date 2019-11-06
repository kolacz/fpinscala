package fpinscala.datastructures

import List._

object Test extends App {

  println(dropWhile(List(1,2,3,4,5,6,7), (x: Int) => x < 5))
  println(dropWhileCurried(List(1,2,3,4,5,6,7))(x => x < 5))
  println(length(List(1,2,3,4,5,6,7,8,9,10,11)))

  def listOfLength(n: Int) = {
    @annotation.tailrec
    def go(n: Int, acc: List[Int]): List[Int] = {
      if (n > 0) go(n - 1, Cons(1, acc))
      else acc
    }
    go(n, Nil)
  }

  // foldRight implementation
  println(length(listOfLength(1000))) 

  // foldLeft implementation 
  println(length2(listOfLength(1000000)))


  println(s"foldLeft sum: ${sum3(List(1,2,3,4,5,6,7,8,9,10))}")
  println(s"foldLeft product: ${product3(List(1,2,3,4,5,6,7,8,9,10))}")

  println(reverse(List(1,2,3,4,5,6,7)))
  println(reverse2(List(1,2,3,4,5,6,7)))

  //foldLeft: ((((((0 - 1) - 2) - 3) - 4) - 5) - 6) - 7 = -28
  //foldRight: 1 - (2 - (3 - (4 - (5 - (6 - (7 - 0)))))) = 4
  println(foldLeftAsFoldRight(List(1,2,3,4,5,6,7), 0)(_ - _))
  println(foldRightAsFoldLeft(List(1,2,3,4,5,6,7), 0)(_ - _))

}
