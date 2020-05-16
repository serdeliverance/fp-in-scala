package lecture.laziness

object NonStrictAndLaziness extends App {

  /* Non-strict parameters are evaluated each time they are referenced in code
     So, they are no cached by Scala
   */
  def maybeTwice(b:Boolean, i: => Int) = if(b) i+i else 0

  val x = maybeTwice(true, { println("hi"); 1+41})

  /* We can cache values if we only want them to be evaluated once, by using
     the lazy keyword
   */
  def maybeTwice2(b:Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }
}
