package lecture.handlingerrors

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]):Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  // def flatMap[B](f: A => Option[B]):Option[B]): Option[B] =
  // map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  def orElse[B >: A](op: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => op
  }

  def filter(f: A => Boolean):Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object HandlingErrorsWithoutExceptions {

  def mean(xs: Seq[Double]):Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  /**
   * flatMap: it similar to map, excepts that the function that we provide to transform the
   * result can itself fail.
   *
   * It is useful to model a computation with multiples stages when any of these can fail. It that case,
   * the computation is abort as soon as the first fail is encountered
   */
  // Ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  // Ex 4.3: my solution
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a, b) match {
    case a == None || b == None => None
    case (Some(elemA), Some(elemB)) => Some(f(elemA,elemB))
  }

  // Ex 4.3: solution from book using flatMap
  def map2b[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)

    map2b(optAge, optTickets)(insuranceRateQuote)
  }
}

object Playground extends App {

  import HandlingErrorsWithoutExceptions._

  val absO = lift(math.abs)

  val abs0result = absO(Some(-2)).getOrElse(throw new Exception("illegal input"))

  println(abs0result)
}
