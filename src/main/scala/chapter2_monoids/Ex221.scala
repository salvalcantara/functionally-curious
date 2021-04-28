package chapter2_monoids

import cats.Functor

/*
Example 2.2.1
 */

object Ex221 {

  /*
  A non-recursive monoid data structure
   */

  trait MonoidF[+A]
  case class Combine[A](x: A, y: A) extends MonoidF[A]
  case object Zero extends MonoidF[Nothing]

  /*
  Show that MonoidF is indeed a functor by defining its map method.
   */

  implicit val functor = new Functor[MonoidF] {
    def map[A, B](ma: MonoidF[A])(f: A => B): MonoidF[B] = ma match {
      case Combine(x, y) => Combine(f(x), f(y))
      case Zero          => Zero
    }
  }

  /*
  Define a non-recursive evaluator for the carrier type Int
   */

  def eval: Algebra[MonoidF, Int] = {
    case Combine(x, y) => x + y
    case Zero          => 0
  }

  def main(args: Array[String]): Unit = {
    println(
      eval(Combine(1, 2)) // 3
    )

    println(
      eval(Zero) // 0
    )
  }
}


