package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def loop(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Empty      => acc.reverse
      case Cons(h, t) => loop(t(), h() :: acc)
    }
    loop(this, Nil)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileR(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionR: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def findF(p: A => Boolean): Option[A] =
    filter(p).headOption

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapU[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.headOption.flatMap(h => Some((f(h), s.drop(1)))))

  def takeU(n: Int): Stream[A] =
    Stream.unfold((this, n)){ case (s, m)  =>
      if (m > 0) s.headOption.flatMap(h =>
        Some((h, (s.drop(1), m - 1)))
      ) else None
    }

  def takeWhileU(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s =>
      s.headOption.flatMap(h => if (p(h)) Some(h, s.drop(1)) else None)
    )

  def zipWith[B >: A, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)){ case (s1, s2) =>
      for (
        h1 <- s1.headOption;
        h2 <- s2.headOption)
      yield (f(h1,h2), (s1.drop(1), s2.drop(1)))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, s2)){ case (st1, st2) =>
      st1.headOption.orElse(st2.headOption)
        .flatMap(_ =>
          Some((st1.headOption, st2.headOption), (st1.drop(1), st2.drop(1)))
        )}

  def zipWith2[B, C](s2: Stream[B])(f: (A,B) => C): Stream[C] = 
    Stream.unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty).forAll{ case (x, y) => x == y }

  def tails: Stream[Stream[A]] =
    unfold(this)(s => s.headOption.flatMap(_ => Some(s, s.drop(1)))) append Stream(empty)

  def hasSubsequence[A](s2: Stream[A]): Boolean =
    tails exists (_ startsWith s2)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, s) => {
      lazy val s1 = s
      val b = f(a, s1._1)
      (b, cons(b, s1._2))
    })._2

  // def scanRight1[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
  //   case Cons(h, t) => val tail = t().scanRight1(z)(f); cons(f(h(), tail.headOption.get), tail)
  //   case Empty => cons(z, empty)
  // }


  // def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
  //   case Cons(_, t) => cons(this.foldRight(z)(f), t().scanRight2(z)(f))
  //   case Empty => cons(z, empty) 
  // }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = cons(0, cons(1, zipWith[Int](_ + _)(fibs, fibs.drop(1))))

  val fibs2: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  def zipWith[A](f: (A, A) => A)(a: Stream[A], b: Stream[A]): Stream[A] = (a, b) match {
    case (Cons(h1, t1), Cons(h2, t2)) => cons(f(h1(), h2()), zipWith(f)(t1(), t2()))
    case _ => Empty
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  val onesU: Stream[Int] = unfold(1)(Some(1,_))

  def constantU[A](a: A): Stream[A] = unfold(a)(Some(a,_))

  def fromU(n: Int): Stream[Int] = unfold(n)((m: Int) => Some((m, m + 1)))

  val fibsU: Stream[Int] = unfold((0,1)){ case (m, n) => Some((m, (n, m + n))) }

}

