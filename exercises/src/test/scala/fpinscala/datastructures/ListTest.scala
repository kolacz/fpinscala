package fpinscala.datastructures

import org.scalatest.FlatSpec
import org.scalatest.Assertions._

class ListTest extends FlatSpec {

  import List._

  "dropWhile & dropWhileCurried" must "drop the prefix of elements for which the precidate is true" in {
    assertResult(List(5,6,7))(dropWhile(List(1,2,3,4,5,6,7), (x: Int) => x < 5))
    assert(dropWhileCurried(List(1,2,3,4,5,6,7))(x => x < 5) == List(5,6,7)) 
  }

  
  // println(lengthR(List(1,2,3,4,5,6,7,8,9,10,11)))

  // def listOfLength(n: Int) = {
  //   @annotation.tailrec
  //   def go(n: Int, acc: List[Int]): List[Int] = {
  //     if (n > 0) go(n - 1, Cons(1, acc))
  //     else acc
  //   }
  //   go(n, Nil)
  // }

  // println(lengthR(listOfLength(1000))) 

  // println(lengthL(listOfLength(1000000)))

  // println(s"foldLeft sum: ${sumL(List(1,2,3,4,5,6,7,8,9,10))}")
  // println(s"foldLeft product: ${productL(List(1,2,3,4,5,6,7,8,9,10))}")

  // println(reverse(List(1,2,3,4,5,6,7)))
  // println(reverseL(List(1,2,3,4,5,6,7)))

  // //foldLeft: ((((((0 - 1) - 2) - 3) - 4) - 5) - 6) - 7 = -28
  // //foldRight: 1 - (2 - (3 - (4 - (5 - (6 - (7 - 0)))))) = 4
  // println(foldLeftR(List(1,2,3,4,5,6,7), 0)(_ - _))
  // println(foldRightL(List(1,2,3,4,5,6,7), 0)(_ - _))

  // println(appendR(List(1,2,3,4,5,6), List(7,8,9,10)))
  // println(appendL(List(1,2,3), List(4,5,6,7,8,9,10)))

  // println(flatten(List(List('a','b','c'), List('d','e','f','g'), List('h', 'i'), Nil, List('j'))))

  // println(add1(List(1,2,3,4,5,6,7,8,9)))
  // println(doubleToStr(List(1.0, 2.0, 3.0)))

  // println(map(List(1,2,3,4,5,6))(_ * 2))
  // println(filter(List(1,2,3,4,5,6))(_ % 2 == 0))
  // println(flatMap(List(1,2,3,4,5,6,7))(i => List(i, i, i)))

  // println(filterFM(List(1,2,3,4,5,6,7,8))(_ % 3 == 2))

  // println(addTwoLists(List(1,2,3), List(4,5,6,7)))
  // println(zipWith(List(1,2,3), List(4,5,6))(_ + _))
  // println(zipWith(List(1,2,3), List(4,5,6))(_.toDouble / _.toDouble))

  // println(hasSubsequence(List(1,2,3,4,5,6,7,8,9), List(8,9))) 
}
