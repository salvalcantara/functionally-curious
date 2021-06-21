package chapter3_monads

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Functor, Monad, ~>}
import chapter1_preliminaries.Ex122.{Coyoneda, freeFunctorForCoyoneda, toCoyoneda}
import chapter3_monads.Greeting.{ConsoleAlg, Println, ProductionInterpreter, Readln}
import chapter3_monads.MyFreeCE.Free
import chapter3_monads.MyFreeCE.Free.{FreeC, foldMapC}

/*
Instead of relying on the Free data type from Cats as in GreetingApp, here there is a simple
implementation for Free (inside MyFreeCE) that can be useful for learning purposes. In particular,
MyFreeCE.Free introduces **explicit** support for the Coyoneda trick via the methods liftFC and
foldMapC. There is also the alias:
- type MyFreeC[F[_], A] = MyFree[Coyoneda[F, *], A]
for the special Coyoneda trick case. The code is mostly a simplification of that in:
https://github.com/scalaz/scalaz/blob/v7.1.0/core/src/main/scala/scalaz/Free.scala
 */

object MyFreeCE {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B])(implicit ff: Functor[F]): Free[F, B] =
      this match {
        case Suspend(s) => Suspend(s map (_ flatMap f)) // "s map ..." <-- That's why we need F to be a functor
        case Pure(a) => f(a)
      }

    def map[B](f: A => B)(implicit ff: Functor[F]): Free[F, B] = flatMap(a => Pure(f(a)))
  }

  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A]

  object Free {
    // Alias that applies Coyoneda on the first argument F
    type FreeC[F[_], A] = Free[Coyoneda[F, *], A]

    def pure[F[_], A](a: => A): Free[F, A] = Pure[F, A](a)

    // Given fa: F[A], it returns Suspend(F(Pure(a)): Free[F, A]
    def liftF[F[_] : Functor, A](fa: => F[A]): Free[F, A] =
      Suspend(
        Functor[F].map(fa)(
          Pure.apply
        )
      )

    // (Coyoneda version) Given fa: F[A], it returns Suspend(Coyoneda[F, *](Pure(a)): MyFreeC[F, A]
    def liftFC[F[_], A](fa: F[A]): FreeC[F, A] = liftF(toCoyoneda(fa))

    /*
    Free Monad homomorphism (or the catamorphism for Free)

    Proceeds by mapping the suspensions with the given natural transformation f: F ~> G,
    and accumulating the results in the target monad G. At the end of the day, the whole
    free structure is collapsed into a value of type G[A].
     */
    def foldMap[A, F[_] : Functor, G[_] : Monad](free: Free[F, A])(f: F ~> G): G[A] =
      free match {
        case Suspend(s) => f(s).flatMap { foldMap(_)(f) }
        case Pure(a) => Monad[G].pure(a)
      }

    /*
    (Coyoneda version) It does the necessary interpreter adaptations when working with an F[_] that is not a functor,
    then calls foldMap.
     */
    def foldMapC[A, F[_], G[_] : Monad](freeC: FreeC[F, A])(f: F ~> G): G[A] = {
      // Using the given f: F ~> G, we need to derive a natural transformation from the Coyoneda[F, *] functor to G.
      val fC = new (Coyoneda[F, *] ~> G) {
        def apply[A](cy: Coyoneda[F, A]): G[A] = {
          f(cy.fx).map(cy.h)
        }
      }

      foldMap(freeC)(fC)
    }
  }
}

object GreetingAppMyFreeCE {
  // Since ConsoleAlg is not a functor, we use MyFreeC in order to get the Coyoneda trick applied
  type Console[A] = FreeC[ConsoleAlg, A]

  // Smart constructors for lifting the ConsoleAlg operations
  def println(msg: String): Console[Unit] = Free.liftFC(Println(msg))
  def readln: Console[String] = Free.liftFC(Readln())

  val program: Console[Unit] = for {
    _     <- println("Please, tell me your name:")
    name  <- readln
    _     <- println(s"Hello $name")
  } yield ()

  def main(args: Array[String]): Unit = {
    foldMapC(program)(ProductionInterpreter) // Produces side effects!
  }
}
