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


object Option {

  import InsuranceApi._

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

  // Ex 4.3: solution from book using flatMap
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))

  // map2 implementation using for comprehensions
  def map2c[A,B,C](a:Option[A],b:Option[B])(f: (A,B)=>C):Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

  // Ex 4.4 with recursion
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  // Ex 4.4 using foldright and map2
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x,y)(_ :: _))

  def parseInts(as: List[String]): Option[List[Int]] =
    sequence(as map (a => Try(a.toInt)))

  // Ex 4.5
  def traverse[A,B](a: List[A])(f:A=>Option[B]):Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A,B](a: List[A])(f:A=>Option[B]):Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t)=>map2(f(h),t)(_ :: _))
}

object InsuranceApi {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???
}
