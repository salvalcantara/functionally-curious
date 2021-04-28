package chapter3_monads

import cats._
import cats.data.State
import cats.free._

/*
 * Comprises Examples 3.2.2, 3.3.1, and 3.3.2
 * Adapted from: https://gist.github.com/msiegenthaler/a5d8a2db7cedd0fd6b93
 */

object Greeting {
  type Console[A] = Free[ConsoleAlg, A]

  sealed trait ConsoleAlg[+A]
  case class Println(msg: String) extends ConsoleAlg[Unit]
  case class Readln() extends ConsoleAlg[String]

  /*
  Smart constructors. In particular, note that Scala's println function is overridden below,
  so in order to use it one needs to specify its full name "scala.Console.println" from now on.
   */

  def println(msg: String): Console[Unit] = Free.liftF(Println(msg))
  def readln: Console[String] = Free.liftF(Readln())

  /*
  Impure interpreter for production (based on direct IO from/to the console)
   */

  object ProductionInterpreter extends (ConsoleAlg ~> Id) {
    override def apply[A](ca: ConsoleAlg[A]): Id[A] = ca match {
      case Println(msg) ⇒ scala.Console.println(msg).asInstanceOf[A]
      case Readln()     ⇒ scala.io.StdIn.readLine().asInstanceOf[A]
    }
  }

  /*
  Pure interpreter for testing purposes (based on the State monad)
   */

  type IO = (List[String], List[String]) // Inputs & Outputs
  type IOState[A] = State[IO, A]

  object TestInterpreter extends (ConsoleAlg ~> IOState) {
    override def apply[A](ca: ConsoleAlg[A]): IOState[A] = ca match {
      case Println(msg) ⇒
        for {
          v <- State.get[IO]
          (is, os) = v
          _ <- State.set((is, os :+ msg))
        } yield ()
      case Readln() ⇒
        for {
          v <- State.get[IO]
          (is, os) = v
          _ <- State.set((is.tail, os))
        } yield is.head
    }
  }

}

object GreetingApp {
  import Greeting._

  val program: Console[Unit] = for {
    _     <- println("Please, tell me your name:")
    name  <- readln
    _     <- println(s"Hello $name")
  } yield ()

  def main(args: Array[String]): Unit = {
    program.foldMap(ProductionInterpreter) // Produces side effects!

    val initialState       = (List("Maria"), Nil)
    val interpretedProgram = program.foldMap(TestInterpreter)
    val finalState         = interpretedProgram.run(initialState)
    scala.Console.println(finalState.value._1)
    // (Nil, List("Please, tell me your name:", "Hello Maria"))
  }
}