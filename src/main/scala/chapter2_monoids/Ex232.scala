package chapter2_monoids

import cats.Functor

/*
Example 2.3.2
 */

object Ex232 {

  /*
  A non-recursive monoid data structure (extended with the Element case)
   */

  trait MonoidF[+A, +R]
  case class  Combine[A, R](x: R, y: R) extends MonoidF[A, R]
  case object Zero                      extends MonoidF[Nothing, Nothing]
  case class  Element[A](x: A)          extends MonoidF[Nothing, Nothing]

  /*
  The corresponding recursive definition is:

    trait Monoid[+A]
    case class Combine[A](x: Monoid[A], y: Monoid[A]) extends Monoid[A]
    case object Zero extends Monoid[Nothing]
   */

  /*
  Coming back to MonoidF, we need to redefine the functor instance
   */

  implicit def functor[A]: Functor[MonoidF[A, *]] = new Functor[MonoidF[A, *]] {
    def map[R, S](ma: MonoidF[A, R])(f: R => S): MonoidF[A, S] = ma match {
      case Combine(x: R, y: R) => Combine(f(x), f(y))
      case Zero                => Zero
      case x @ Element(_)      => x
    }
  }

  def eval1: Algebra[MonoidF[Int, *], Int] = {
    case Combine(x, y)   => x + y
    case Zero            => 0
    case Element(x: Int) => x
  }

  def eval2: Algebra[MonoidF[Int, *], String] = {
    case Combine(x, y)   => s"$x + $y"
    case Zero            => "0"
    case Element(x: Int) => s"$x"
  }

  /*
  Type alias and smart constructors for making it easier to build expressions
   */

  type Mon[A] = Fix[MonoidF[A, *]]
  def add[A](x: Mon[A], y: Mon[A]): Mon[A] = Fix(Combine(x, y))
  def zero[A]: Fix[MonoidF[A, *]] = Fix[MonoidF[A,*]](Zero)
  def num[A](x: A): Mon[A] = Fix[MonoidF[A, *]](Element[A](x))

  def main(args: Array[String]): Unit = {
    val originalExpr: Fix[MonoidF[Int, *]] = Fix(Combine(
      Fix[MonoidF[Int, *]](Zero),
      Fix(Combine(
        Fix[MonoidF[Int, *]](Element(1)),
        Fix[MonoidF[Int, *]](Element(2))
      ))
    ))

    /*
    Obtain the same expression using the smart constructors above (add, zero, num)
     */

    val expr = add(zero, add(num(1), num(2))) // Better ;-)
    assert(expr == originalExpr) // Must be equal

    /*
    Obtain recursive evaluators automatically using cata
     */

    val recEval1 = cata(eval1)    // Automatic recursive evaluator!
    val result1  = recEval1(expr) // 3, the sum of all the elements
    println(result1)

    val recEval2 = cata(eval2)    // Automatic recursive evaluator!
    val result2  = recEval2(expr) // "0 + 1 + 2", the "sum" of all the elements
    println(result2)
  }
}
