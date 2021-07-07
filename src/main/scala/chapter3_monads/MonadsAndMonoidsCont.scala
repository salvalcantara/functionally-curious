package chapter3_monads

import cats.{Functor, Id, ~>}
import chapter3_monads.MonadsAndMonoids.{FunctorComposition, HigherMonoid}
import chapter3_monads.MonadsAndMonoidsCont.Monad

object MonadsAndMonoidsCont {
  /*
  We can repeat everything, but this time taking the Monad definition based on flatMap and pure
   */
  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](a: A): F[A]

    // Default implementation of map and flatten in terms of flatMap and pure
    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  }
}

object MonadFromHigherMonoid {
  /*
  Given "a monoid in the category of endofuctors", we can always define a monad.
   */
  abstract class MonadFromHigherMonoid[F[_]: Functor]
    extends HigherMonoid[F, ~>, FunctorComposition[F, F, *], Id] with Monad[F] {
    // Requires to implement the HigherMonoid trait: μ and η (combine and empty)

    /*
    Default implementation of the Monad trait (flatMap and pure) in terms of combine and empty
     */
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = combine(Functor[F].map(fa)(f))
    def pure[A](a: A): F[A] = empty(a)
  }

  /*
  Example for List
   */
  object ListMonad extends MonadFromHigherMonoid[List] {
    override val μ = new (FunctorComposition[List, List, *] ~> List) {
      def apply[A](lla: List[List[A]]): List[A] = lla.flatten // call built-in flatten for Lists
    }

    override val η = new (Id ~> List) {
      def apply[A](a: Id[A]) = List(a)
    }
  }

  def main(args: Array[String]): Unit = {
    println(
      ListMonad.μ(List(List(1, 2, 3))) // List(1, 2, 3)
    )
    println(
      ListMonad.η(1) // List(1)
    )
    println(
      ListMonad.flatMap(List(1, 2, 3))(n => List.fill(n)(n)) // List(1, 2, 2, 3, 3, 3)
    )
    println(
      ListMonad.pure(1) // List(1)
    )
  }
}

object HigherMonoidFromMonad {
  /*
  And also the other way around, given a monad, we can always define a
  "monoid in the category of endofunctors"
 */
  abstract class HigherMonoidFromMonad[F[_]: Functor]
    extends Monad[F] with HigherMonoid[F, ~>, FunctorComposition[F, F, *], Id] {
    // Requires to implement the Monad trait: flatMap and pure

    /*
    Default implementation of μ and η in terms of flatten (which is in turn defined in terms of
    flatMap within the Monad trait) and pure
     */
    val μ = new (FunctorComposition[F, F, *] ~> F) {
      def apply[A](fa: F[F[A]]): F[A] = flatten(fa)
    }

    val η = new (Id ~> F) {
      def apply[A](a: Id[A]) = pure(a)
    }
  }

  /*
  Example for List
   */
  object ListMonad extends HigherMonoidFromMonad[List] {
    override def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la.flatMap(f)
    override def pure[A](a: A): List[A] = List(a)
  }

  def main(args: Array[String]): Unit = {
    println(
      ListMonad.μ(List(List(1, 2, 3))) // List(1, 2, 3)
    )
    println(
      ListMonad.η(1) // List(1)
    )
    println(
      ListMonad.flatMap(List(1, 2, 3))(n => List.fill(n)(n)) // List(1, 2, 2, 3, 3, 3)
    )
    println(
      ListMonad.pure(1) // List(1)
    )
  }
}
