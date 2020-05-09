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

class HandlingErrorsWithoutExceptions extends App {

}

object HandlingErrorsWithoutExceptions {

  def mean(xs: Seq[Double]):Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  // Ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
   * flatMap: it similar to map, excepts that the function that we provide to transform the
   * result can itself fail.
   *
   * It is useful to model a computation with multiples stages when any of these can fail. It that case,
   * the computation is abort as soon as the first fail is encountered
   */
}
