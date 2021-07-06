package chapter3_monads

import cats.{Functor, Id, ~>}
import chapter3_monads.MonadsAndMonoids.{FunctorComposition, HigherMonoid, Monad}

object MonadsAndMonoids {

  /*
  A monad is just a monoid in the category of endofunctors
  —Saunders Mac Lane

  As a supplement to Section 3.1, we will show, in code, the equivalence between monads and this
  notion of (higher) monoids. Hopefully, this will help you understand the above famous quote.

  See also:
  - https://gist.github.com/mandubian/dfd670f7740f47a1a2a7b662f828aac6
  - https://blog.rockthejvm.com/monads-are-monoids-in-the-category-of-endofunctors/
    - https://twitter.com/rockthejvm/status/1379695298365300736 (short version)
    - https://www.youtube.com/watch?v=CMm98RkCgPg (video version)
  which served as the main sources of inspiration.
   */

  /*
  Let's remind the definition of a Monad first (based on flatMap and pure)
   */
  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](a: A): F[A]

    // Default implementation of map and flatten in terms of flatMap and pure
    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  }

  /*
  Now, consider this generalized definition of monoid (https://en.wikipedia.org/wiki/Monoid_(category_theory))
   */
  trait HigherMonoid[F[_], →[_[_], _[_]], FxF[_], I[_]] {
    def μ: FxF → F // The so-called "multiplication"
    def η:   I → F // The so-called "unit"

    @inline def combine = μ // The multiplication corresponds to the "combine" operation
    @inline def empty   = η // The unit corresponds to the "empty" element
  }

  /*
  Define a type for representing "functor composition", which we will also need
   */
  type FunctorComposition[F[_], G[_], A] = F[G[A]] // Conceptually F[G[_]]
  /*
  Note that by functor we really mean endofunctor, since in Scala we just have one category:
  the category of types. So, every Functor (F[_], G[_], ...) is really an endofunctor in code.
  Since the origin and target categories are the same, endofunctors always compose.

  Note also that the above definition just requires F[_] and G[_] to be type constructors
  (first-order kinds), not Functor's. However, in order to match mathematics as closely as
  possible, we can just assume that there will be Functor instances for them.
   */
}

object MonadFromHigherMonoid {
  /*
  Given "a monoid in the category of endofuctors", we can always define a monad.

  By "the category of endofunctors", this is what we mean:
  - The category where:
    - objects are endofunctors (F[_]: Functor)
    - morphisms are natural transformations (~>)

  By "a monoid in the category of endofunctors", this is what we mean:
  - A class/object
    - having an F[_]: Functor
    - and extending HigherMonoid[F, ~>, FunctorComposition[F, F, *], Id]
   */
  abstract class MonadFromHigherMonoid[F[_]: Functor]
    extends HigherMonoid[F, ~>, FunctorComposition[F, F, *], Id] with Monad[F] {
    /*
    Requires to implement the HigherMonoid trait: μ and η (combine and empty)
     */

    /*
    Default implementations of the Monad trait (pure and flatMap) in terms of combine and empty
     */
    def pure[A](a: A): F[A] = empty(a)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = combine(Functor[F].map(fa)(f))
  }

  /*
  Example for List
   */
  object ListMonad extends MonadFromHigherMonoid[List] {
    val μ: FunctorComposition[List, List, *] ~> List = new (FunctorComposition[List, List, *] ~> List) {
      def apply[A](lla: List[List[A]]): List[A] = lla.flatten // call built-in flatten for Lists
    }

    val η: Id ~> List = new (Id ~> List) {
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
    /*
    Requires to implement the Monad trait: flatMap and pure
     */

    /*
    Default implementation of μ and η in terms of flatten (which is defined in terms of flatMap
    within the Monad trait) and pure
     */
    val μ: FunctorComposition[F, F, *] ~> F = new (FunctorComposition[F, F, *] ~> F) {
      def apply[A](fa: F[F[A]]): F[A] = flatten(fa)
    }

    val η: Id ~> F = new (Id ~> F) {
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
