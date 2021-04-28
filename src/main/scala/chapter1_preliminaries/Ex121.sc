import cats.Functor

/*
Example 1.2.1
 */

trait Yoneda[F[_], A] {
  def beta[X](h: A => X): F[X]
}

/*
The following pair of functions "prove" the Yoneda lemma:
  "Yoneda[F, A] is isomorphic to F[A]"
 */

def fromYoneda[F[_], A](y: Yoneda[F, A]): F[A] = y.beta(identity) // key point!

def toYoneda[F[_]:Functor, A](fa: F[A]): Yoneda[F, A] = new Yoneda[F, A] {
  def beta[X](h: A => X): F[X] = Functor[F].map(fa)(h)
}
