/*
Example 1.1.8
 */

/*
Conceptually, a natural transformation, let's call it alpha, is a parametric
(universally quantified) function of the form:

  def alpha[A]: F[A] => G[A]

where F[_] are G[_] are type constructors (or functors, for what it's worth).
 */

/*
A typical example is headOption.
 */

def headOption[A]: List[A] => Option[A] = {
  case head :: tail => Some(head) // :: (the Cons operator) prepends an element to a list
  case Nil => None // Nil is the empty list, or List()
}

headOption(List(1, 2, 3))       // Some(1)
headOption(List("1", "2", "3")) // Some("1")

/*
As a matter of fact, headOption is part of the Scala standard library and one can simply do:
 */

List(1,2,3).headOption         // Some(1)
List("1", "2", "3").headOption // Some("1")
