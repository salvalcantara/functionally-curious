package chapter2_monoids

import cats.Functor

/*
Example 2.2.1
 */

object Ex222 {

  /*
  A non-recursive grammar for arithmetic expressions
   */

  sealed trait ExpressionF[+A]
  case class Add[A](x: A, y: A) extends ExpressionF[A]
  case class Mul[A](x: A, y: A) extends ExpressionF[A]
  case class Neg[A](x: A) extends ExpressionF[A]
  case object Zero extends ExpressionF[Nothing]
  case object One extends ExpressionF[Nothing]

  /*
  Define two evaluators for the carrier types Int and String, respectively
   */

  def eval1: Algebra[ExpressionF, Int] = {
    case Add(x, y) => x + y
    case Mul(x, y) => x*y
    case Neg(x)    => -x
    case Zero      => 0
    case One       => 1
  }

  def eval2: Algebra[ExpressionF, String] = {
    case Add(x, y) => s"$x + $y"
    case Mul(x, y) => s"$x * $y"
    case Neg(x)    => s"-$x"
    case Zero      => "0"
    case One       => "1"
  }

  def main(args: Array[String]): Unit = {
    println(
      eval1(Add(1, 2)) // 3
    )
    println(
      eval2(Add("1", "2")) // "1 + 2"
    )

    println(
      eval1(One) // 1
    )
    println(
      eval2(One) // "1"
    )
  }
}
