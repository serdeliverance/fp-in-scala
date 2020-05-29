package lecture.purelyfunctionalstate

import scala.annotation.tailrec

object PurelyFunctionalState {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    // Ex 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    // Ex 6.2
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = rng.nextInt
      (if (i == Int.MaxValue) (i - 1)/Int.MaxValue else i, r)
    }

    // Ex 6.3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r1) = intDouble(rng)
      ((d, i), r1)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1,d2,d3), r3)
    }

    // Ex 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

      @tailrec
      def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
        if (count == 0) (xs, r)
        else {
          val (x, r2) = r.nextInt
          go(count - 1, r2, x :: xs)
        }
      }

      go(count, rng, List())
    }

    def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count == 0)
        (List(), rng)
      else {
        val (x, r1)  = rng.nextInt
        val (xs, r2) = ints(count - 1)(r1)
        (x :: xs, r2)
      }

    // we define a type alias. It allow us to using combinators later
    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    // our first combinator. It allow us to pasing state without having to explicitly passing along
    // the RNG state
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

    // Ex 6.5
    def doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => if (i == Int.MaxValue) (i - 1)/Int.MaxValue else i)

    def doubleViaMapB: Rand[Double] = map(nonNegativeInt)(_/ (Int.MaxValue.toDouble + 1))

    // Ex 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a,b), rng3)
      }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)((_,_))

    val randIntDouble: Rand[(Int,Double)] = both(int,double)

    val randDoubleInt: Rand[(Double,Int)] = both(double,int)

    // Ex 6.7
    // TODO: Hard

    // Ex 6.8
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    // Ex 6.9
    // TODO
    def mapB[A, B](s: Rand[A])(f: A => B): Rand[B] = ???
    def map2b[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = ???

    // Ex 6.10
    // TODO

    // Ex 6.11
    // TODO Hard
  }
}

object Playground extends App {

  import PurelyFunctionalState._

  val simpleRNG = new SimpleRNG(23)

  val (randomDouble, _) = simpleRNG.double(simpleRNG)

  println(randomDouble)

  val anoterRNG = new SimpleRNG(41)
  val double3Result = simpleRNG.double3(anoterRNG)

  println(double3Result)

  val nIntsResult = simpleRNG.ints(5) (anoterRNG)
  println(nIntsResult)
}
