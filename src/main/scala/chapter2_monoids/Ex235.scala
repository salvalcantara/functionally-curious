package chapter2_monoids

import cats.Functor

/*
Example 2.3.5
 */

object Ex235 {

  /*
  A non-recursive definition for lists
   */

  sealed trait ListF[+A, +R]
  case class ConsF[A, R](head: A, tail: R) extends ListF[A, R]
  case object NilF extends ListF[Nothing, Nothing]

  /*
  A functor instance and an evaluator
   */

  implicit def functorListF[A]: Functor[ListF[A, *]] = new Functor[ListF[A, *]] {
    def map[R, S](fa: ListF[A, R])(f: R => S): ListF[A, S] = fa match {
      case ConsF(head, tail) => ConsF(head, f(tail))
      case NilF              => NilF
    }
  }

  def eval: Algebra[ListF[Int, *], Int] = {
    case ConsF(h, t)   => h + t
    case NilF          => 0
  }

  /*
  Let's also consider the recursive definition for a list
   */

  sealed trait List[+A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  def main(args: Array[String]): Unit = {
    /*
    A list containing the elements 1, 2, and 3
     */

    val list1: Fix[ListF[Int, *]] =
      Fix(ConsF(1,
        Fix(ConsF(2,
          Fix(ConsF(3,
            Fix[ListF[Int, *]](NilF)
          ))
        ))
      ))

    /*
    Use cata to obtain a recursive evaluator
     */

    val recEval1 = cata(eval)   // Automatic recursive evaluator!
    val result1 = recEval1(list1) // 6, the sum of all the elements
    println(result1)

    /*
    For lists, however, we have specialized (adhoc) catamorphisms called folds. E.g.,
     */

    def foldRight[A, B](la: List[A], z: B)(op: (A, B) => B): B = la match {
      case Cons(head, tail) => op(head, foldRight(tail, z)(op))
      case Nil              => z
    }

    val list2: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    val result2 = foldRight(list2, 0)((head, acc) => head + acc) // 6
    println(result2)

    /*
    Of course, one can simply rely on the definitions and methods built in the Scala standard
    library
     */

    val list3 = List(1, 2, 3)
    val result3 = list3.foldRight(0)((head, acc) => head + acc) // 6
    println(result3)

    /*
    In fact, for this particular case, one would simply do
     */

    val result4 = list3.sum // 6
    println(result4)
  }
}
