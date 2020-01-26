package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _}

object EitherTest extends App {

  import fpinscala.errorhandling.Either._
  println(Right(10) map (_ + 100))
  println(Left("error!") map ((x: Int) => x + 100))
  println(Right("10") flatMap ((s: String) => Try(s.toInt)))
  println(Right("1 0") flatMap ((s: String) => Try(s.toInt)))
  println(Left("error") flatMap ((s: String) => Try(s.toInt)))
  println(Left("error") orElse Right(10))
  println(Right(10).map2(Right(12))((x,y) => x + y))
  println(Right("abc").map2(Left("error"))((x,y) => x + y))
  println(traverse(List(10,20,30))((d: Int) => Try(60 / d)))
  println(traverse(List("1", "2", "abc", "0", "def"))((s: String) => Try(10 / s.toInt)))
}
