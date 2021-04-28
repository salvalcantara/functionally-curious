import cats.Functor

/*
Example 1.1.6
 */

/*
List is a functor, and as such, we can map over it.
 */

List(1, 2, 3).map(x => x + 1) // List(2, 3, 4)

/*
Mapping over a list using Cats
 */

val f = (x: Int) => x + 1

Functor[List].map(List(1, 2, 3))(f)  // List(2, 3, 4)
Functor[List].lift(f)(List(1, 2, 3)) // List(2, 3, 4)