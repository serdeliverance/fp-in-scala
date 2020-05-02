package lecture.datastructures

import scala.annotation.tailrec

object FunctionalDatastructures extends App {
  import List._

  val testElements = List("scala", "swift", "akka", "play", "lagom", "java", "spring")

  val resultDrop = drop(testElements, 3)
  println(s"drop result: $resultDrop")

  val resultDropWhile = dropWhile(testElements, (e: String) => e.startsWith("s"))
  println(s"dropWhile result: $resultDropWhile")

  // with this version is not needed to specify x type in the left side of the lambda
  val resultDropWhile2 = dropWhile2(testElements) (x => x.startsWith("s"))

  val resultLength = length(testElements)
  println(s"length: $resultLength")

  val numbers = List(1, 2, 3, 4)
  val resultFoldLeft = foldLeft(numbers, 0)(_ + _)
  println(s"foldLeft result: $resultFoldLeft")
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Double]): Double = ints match {
    case Nil => 1.0
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Ex 3.2 tail function
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new IllegalArgumentException("tail of an empty list")
    case Cons(_, xs) => xs
  }

  // Ex 3.3 set head
  def setHead[A](newHead: A, as: List[A]): List[A] = as  match {
    case Nil => List(newHead)
    case Cons(_, t) => Cons(newHead, t)
  }

  // Ex 3.4 drop (using tail)
  def drop[A](as: List[A], n: Int): List[A] = {
    @tailrec
    def helper(as: List[A], n: Int): List[A] = {
      if (n == 0) as
      else helper(tail(as), n - 1)
    }

    helper(as, n)
  }

  // Ex 3.5 dropWhile
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A], p: A => Boolean): List[A] = as match {
        case Nil => as
        case Cons(x, xs) => if (!p(x)) as else go(xs, p)
      }

    go(as, p)
  }

  // sample: dropWhile with currying (improving type inference)
  def dropWhile2[A] (as: List[A]) (p: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A], p: A => Boolean): List[A] = as match {
      case Nil => as
      case Cons(x, xs) => if (!p(x)) as else go(xs, p)
    }

    go(as, p)
  }

  // Ex 3.6
  // TODO

  // generalizing functions with HOF and using currying for improving type inference
  def foldRight[A,B](as: List[A], z: B) (f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z) (f))
    }
  }

  def sumF(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def productF(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  // Ex 3.7
  // Answer: It can't. It needs to traves all the structure before returning, no matter if
  // it gets a zero value in the middle.

  // Ex 3.9
  def length[A](as: List[A]): Int = {
    @tailrec
    def go(as: List[A], acc: Int): Int = {
      as match {
        case Nil => acc
        case Cons(_, xs) => go(xs, acc + 1)
      }
    }

    go(as, 0)
  }

  // Ex 3.10: foldLeft tail recursive
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Ex 3.11 write a sum and prod function that uses foldLeft
  def sumFL(as: List[Int]): Double = {
    foldLeft(as, 0)(_ + _)
  }

  def productFl(as: List[Int]): Double = {
    foldLeft(as, 1)(_ * _)
  }

  // Ex 3.12 function that reverse a list
  def reverse[A](as: List[A]): List[A] = ???
  // TODO

  // Ex 3.13 HARD
  // TODO

  // Ex 3.14
  // TODO

  // Ex 3.15
  // TODO

  // Ex 3.16 up to 3.23
  // TODO: all of them

  // Ex 3.24 HARD
  // TODO
}
