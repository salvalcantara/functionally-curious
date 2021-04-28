import cats.Functor

package object chapter2_monoids {

  /*
  Fix and unFix (or Lambek's lemma)
   */

  case class Fix[F[_]](x: F[Fix[F]])
  def unFix[F[_]]: Fix[F] => F[Fix[F]] = { case Fix(x) => x }

  /*
  The definition of an F-algebra
   */

  type Algebra[F[_], A] = F[A] => A

  /*
  Definition catamorphism, or cata, in both short/long forms
   */

  def cata[F[_]:Functor, A](eval: F[A] => A): Fix[F] => A =
    eval compose (Functor[F].map(_: F[Fix[F]])(cata(eval))) compose unFix

  def cataLong[F[_]:Functor, A](eval: F[A] => A): Fix[F] => A = expr => {
    val ff:    Functor[F] = implicitly[Functor[F]]

    val one:   F[Fix[F]]  = unFix(expr)
    val two:   F[A]       = ff.map(one)(cataLong(eval))
    val three: A          = eval(two)

    three
  }
}
