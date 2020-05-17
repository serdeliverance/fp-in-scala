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
  // TODO

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
}