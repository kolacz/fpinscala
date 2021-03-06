package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = int(rng)
    val abs = (((n >> 31) & (-n)) + ((~(n >> 31)) & n))
    (abs, rng2)
  }

  // def double(rng: RNG): (Double, RNG) = {
  //   val (n, rng2) = int(rng)
  //   val min = Int.MinValue.toDouble
  //   val x = (n - min - 1)/(Int.MaxValue - min)
  //   (x, rng2)
  // }

  def double: Rand[Double] = {
    val min = Int.MinValue.toDouble
    map(int)(n => (n - min - 1)/(Int.MaxValue - min))
  }

  // def intDouble(rng: RNG): ((Int,Double), RNG) = {
  //   val (n, rng2) = int(rng)
  //   val (x, rng3) = double(rng2)
  //   ((n, x), rng3)
  // }

  // def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  //   val (x, rng2) = double(rng)
  //   val (n, rng3) = int(rng2)
  //   ((x, n), rng3)
  // }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (x1, rng2) = double(rng)
    val (x2, rng3) = double(rng2)
    val (x3, rng4) = double(rng3)
    ((x1,x2,x3), rng4)
  }

  // def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //   val (n, rng2) = int(rng)
  //   val (tail, rng3) = if (count > 1) ints(count - 1)(rng2) else (Nil, rng2)
  //   (n :: tail, rng3)
  // }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  def intDouble: Rand[(Int,Double)] =
    both(int, double)

  def doubleInt: Rand[(Double,Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]] (unit(Nil)) { case (rng, acc) =>
      map2(rng, acc)(_ :: _)
    }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
