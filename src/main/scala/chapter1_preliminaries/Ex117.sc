import cats.Functor

/*
Example 1.1.7
 */

/*
Option is a functor, too. The important thing to note is that there are more things that can be
mapped over, not just lists!
 */

val o = Option(123) // Some(123)
Functor[Option].map(o)(_.toString) // Some("123")

/*
_.toString is a shortcut (an anonymous/lambda function, indeed) for:

  val f: Int => String = x => x.toString

or if your prefer:

  def f(x: Int): String = x.toString
 */