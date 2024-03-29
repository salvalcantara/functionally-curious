package chapter3_monads

import cats.{Monad, ~>}
import chapter3_monads.Greeting.{ConsoleAlg, Println, ProductionInterpreter, Readln}
import chapter3_monads.MyFreeCI.{Free, Pure, Suspend}
import chapter3_monads.MyFreeCI.Free.foldMap

/*
Here we provide another simple implementation (in MyFreeCI) for Free which makes **implicit** use
of the Coyoneda trick. In particular, the user does not even need to worry about this. Essentially,
MyFreeCI.Free[F, A] is equivalent to MyFreeCE.FreeC[F, A] == MyFreeCE.Free[Coyoneda[F, *], A]. For
more on the equivalence, see: http://blog.higher-order.com/blog/2013/11/01/free-and-yoneda/

The code is along the lines of that in:
https://www.youtube.com/watch?v=cxMo1RMsD0M and https://www.youtube.com/watch?v=M258zVn4m2M, which
can be regarded as a much simplified version of the one in Cats:
https://github.com/typelevel/cats/blob/main/free/src/main/scala/cats/free/Free.scala
 */

object MyFreeCI {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      this match {
        case Suspend(fa, c) => Suspend(fa, c andThen (_ flatMap f))
        case Pure(a) => f(a)
      }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Pure(f(a)))
  }

  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], X, A](fx: F[X], c: X => Free[F, A]) extends Free[F, A]

  object Free {
    def pure[F[_], A](a: => A): Free[F, A] = Pure[F, A](a)

    def liftF[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa, Pure.apply)

    def foldMap[A, F[_], G[_] : Monad](free: Free[F, A])(f: F ~> G): G[A] =
      free match {
        case Suspend(fa, c) => Monad[G].flatMap(f(fa)) { a => foldMap(c(a))(f) }
        case Pure(a) => Monad[G].pure(a)
      }
  }
}

object GreetingAppMyFreeCI {
  type Console[A] = Free[ConsoleAlg, A]

  // Smart constructors for lifting the ConsoleAlg operations
  def println(msg: String): Console[Unit] = Free.liftF(Println(msg))
  def readln: Console[String] = Free.liftF(Readln())

  val program: Console[Unit] = for {
    _     <- println("Please, tell me your name:")
    name  <- readln
    _     <- println(s"Hello $name")
  } yield ()

  // Basically, program should contain the following AST (or program description as a value)
  val expectedProgram: Console[Unit] =
    Suspend(Println("Please, tell me your name"), (_: Any) =>
      Suspend(Readln(), (name: String) =>
        Suspend(Println(s"Hello $name"), (_: Any) =>
          Pure( () )
        )
      )
    )

  def main(args: Array[String]): Unit = {
    foldMap(program)(ProductionInterpreter) // Produces side effects!
    foldMap(expectedProgram)(ProductionInterpreter) // Should behave identically
  }
}
