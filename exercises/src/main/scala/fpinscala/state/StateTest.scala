package fpinscala.state

import RNG._

object StateTest extends App {

  type Rand[+A] = RNG => (A, RNG)

  def testMultiple(foo: RNG => (Any, RNG), nTimes: Int = 20): Unit = {
    val rng0 = Simple(42)

    def loop(rng: RNG, n: Int): Unit = {
      if (n >= 0) {
        val (m, rng2) = foo(rng)
        println(m)
        loop(rng2, n - 1)
      }
    }
    loop(rng0, nTimes)
  }

  // nonNegativeIntTest

  // testMultiple(RNG.nonNegativeInt _)
  // testMultiple(RNG.double _)
  // testMultiple(RNG.intDouble _)
  // testMultiple(RNG.doubleInt _)
  // testMultiple(RNG.double3 _)

  // testMultiple(RNG.ints(10) _ )

  // testMultiple(RNG.nonNegativeEven)

  testMultiple(ints(10))
}
