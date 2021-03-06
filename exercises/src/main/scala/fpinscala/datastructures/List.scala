package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1) 
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def dropWhileCurried[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (t == Nil) t else Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumR(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def productR(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def lengthR[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => 1 + x)

  def sumL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productL(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(k: List[A], acc: List[A]): List[A] =
      k match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, Cons(x, acc))
      }

    loop(l, Nil)
  }

  def reverseL[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldLeftR[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((a, b) => f(b, a))

  def foldRightL[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, z)((b, a) => f(a, b))

  def appendR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def appendL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc, x) => Cons(x, acc))

  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendR(_,_))

  def add1(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def doubleToStr(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(d, ds) => Cons(d.toString, doubleToStr(ds))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => appendR(f(x), flatMap(xs)(f))
  }

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def addTwoLists(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addTwoLists(xs, ys))
    case _ => Nil
  }

  def zipWith[A,B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = (a1, a2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    case _ => Nil
  }

  // x :: xs (ends with colon so it's right-associative) => xs.::(x) => ::(x, xs)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    val Cons(head, tail) = sub

    def aux[A](a1: List[A], a2: List[A]): Boolean =
      (a1, a2) match {
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y) aux(xs, ys)
          else if (x == head) aux(xs, tail)
          else aux(xs, sub)
        case (_, Nil) => true
        case _ => false
      }

    aux(sup, sub)
  }

}
