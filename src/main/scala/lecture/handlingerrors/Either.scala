package lecture.handlingerrors

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  import InsuranceApi._

  def mean(xs: Seq[Double]):Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int):Either[Exception, Double] =
    try Right(x/y)
    catch { case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  def parseInsuranceRateQuote(age:String, numberOfSpeedingTickets:String): Either[Exception, Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)

  // Ex: 4.7: implement sequence and traverse for Either
  // TODO
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = ???
}

object Person {

  // listing 4.4 Using Either to validate data
  case class Person(name:Name, age:Age)
  case class Name(val value:String)
  case class Age(val value:Int)

  def mkName(name:String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty")
    else Right(Name(name))

  def mkAge(age:Int): Either[String,Age] =
    if (age < 0) Left("age is out of range")
    else Right(Age(age))

  def mkPerson(name:String, age:Int): Either[String,Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))

  def mkPerson2(name:String, age:Int): Either[String,Person] =
    for {
      n <- mkName(name)
      a <- mkAge(age)
    } yield Person(n,a)

  // Ex 4.8
  // TODO
}