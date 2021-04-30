package chapter1_preliminaries

import cats.Functor

/*
Example 1.2.2
 */

object Ex122 {

  trait Coyoneda[F[_], A] {
    type X
    def h: X => A
    def fx: F[X]
  }

  /*
The following pair of functions "prove" the Co-Yoneda lemma:
  "Coyoneda[F, A] is isomorphic to F[A]"
 */

  def fromCoyoneda[F[_]: Functor, A](cy: Coyoneda[F, A]): F[A] = Functor[F].map(cy.fx)(cy.h)

  def toCoyoneda[F[_], A](fa: F[A]): Coyoneda[F, A] = new Coyoneda[F, A] {
    type X = A
    val h = identity
    val fx = fa
  }

  /*
  The so-called coYoneda trick consists of building a functor, Coyoneda[F, *], from any given type
  constructor F[_].
   */

  implicit def freeFunctorForCoyoneda[F[_]]: Functor[Coyoneda[F, *]] = new Functor[Coyoneda[F, *]] {
    def map[A, B](cy: Coyoneda[F, A])(f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
      type X = cy.X
      val h = f compose cy.h // map fusion *for free*!
      val fx = cy.fx
    }
  }

}
