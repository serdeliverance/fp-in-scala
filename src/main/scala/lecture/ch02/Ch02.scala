package lecture.ch02

import scala.annotation.tailrec

object Ch02 extends App{

  def findFirst[A](p: A => Boolean, elements: List[A]) = {
    @tailrec
    def helper(n: Int, elements: List[A] ): Int = {
      if (n < 0) -1
      else if (p(elements(n))) n
      else helper(n - 1, elements)
    }
    helper(elements.size - 1, elements)
  }

  val list = List("house", "guitar", "chair", "quarantine")

  println(findFirst((a: String) => a == "sergio", list))
  println(findFirst((a: String) => a == "house", list))
  println(findFirst((a: String) => a == "guitar", list))

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean) : Boolean = {

    def helper(n: Int): Boolean = {
      if (n == 0) true
      else if (!ordered(as(n),as(n-1))) false
      else helper(n - 1)
    }

    helper(as.size - 1)
  }

  val sampleList = List("te", "apple", "magazine", "Brazil")
  val sampleList2 = List("apple", "banana", "computation", "scala")

  println(isSorted(sampleList, (a: String, b: String) => a.compareTo(b) > 0))
  println(isSorted(sampleList2, (a: String, b: String) => a.compareTo(b) > 0))

  class PowYou {
    def apply(number: Double) = number * number
  }

  val powYou = new PowYou()

  println(powYou(3.24))

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)
}
