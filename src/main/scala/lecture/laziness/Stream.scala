package lecture.laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // Ex: 5.1
  // TODO
  def toList: List[A] = ???

  // Ex: 5.2
  // TODO

  // Ex: 5.3
  // TODO

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // defining exists in terms of foldRight
  def exists2(p: A => Boolean): Boolean =
    foldRight(false) ((a,b) => p(a) || b)

  // Ex: 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) =>
      if (!p(h())) false
      else t().forAll(p)
    case _ => true
  }

  def forAll2(p: A=> Boolean): Boolean =
    foldRight(false)((a,b) => p(a) && b)

  // Ex: 5.5
  // TODO

  // Ex: 5.6
  // TODO

  // Ex: 5.7
  // TODO
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:() => A, t:() => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  // Example: infinte stream
  val ones: Stream[Int] = Stream.cons(1, ones)

  // Ex: 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // Ex: 5.9
  def from(n: Int):Stream[Int] = Stream.cons(n, from(n+1))

  // Ex: 5.10
  val fibs = {
    def go(prev:Int, curr: Int) : Stream[Int] = {
      Stream.cons(prev, go(curr, prev + curr))
    }
    go(0,1)
  }

  // Ex: 5.11
  // TODO: review
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  // Ex: 5.12
  val fibs2 = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  val ones2 = unfold(1)(_ => Some((1,1)))

  def constant2[A](a: A) = unfold(a)(_ => Some((a, a)))

  def from2(n: Int) = unfold(n)(_ => Some(n, n + 1))

  // Ex: 5.13
  // TODO

  // Ex: 5.14
  // TODO Hard

  // Ex: 5.15
  // TODO

  // Ex: 5.16
  // TODO Hard
}