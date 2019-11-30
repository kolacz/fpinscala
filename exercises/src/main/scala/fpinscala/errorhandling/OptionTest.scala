package fpinscala.errorhandling

import scala.{Option => _, Some => _, None => _, Either => _, _}
import fpinscala.errorhandling.Option._

object OptionTest extends App {
  println(Some(1).map(_ * 2))
  println(None.getOrElse(println(1)))
  println(Some(2).flatMap(x => if (x == 10) Some(x) else None))
  println(List(1,2,3,4,5,6,7,8).map(Some(_)).map(_.filter(_ % 2 == 0)).map(_.filter(_ < 7)))
  println(mean(List(1.0, 2.0, 3.0, 4.0, 5.7421)))
  println(mean(Nil))
}
