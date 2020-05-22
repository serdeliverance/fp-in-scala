package lecture.purelyfunctionalstate

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
  }
}

object Playground extends App {

  import PurelyFunctionalState._

  val simpleRNG = new SimpleRNG(23)

  val (randomDouble, _) = simpleRNG.double(simpleRNG)

  println(randomDouble)
}
