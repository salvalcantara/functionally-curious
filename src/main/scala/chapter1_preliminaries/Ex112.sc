/*
Example 1.1.2
 */

/*
Define a function for determining whether a given number is even or not.
 */

val isEven: Int => Boolean = x => x % 2 == 0
// def isEven: Int => Boolean = x => x % 2 == 0 // Alternative definition as a method
// def isEven(x: Int): Boolean = x % 2 == 0     // More typically written like this

isEven(1) // false
isEven(2) // true

/*
Define an identity function.
 */

def id[A](x: A): A = x

id(1) // 1
id(2) // 2
id("asdf") // "asdf"

/*
Note, Scala already provides an identity function, so it's not strictly necessary to define your
own one.
 */

identity(1) // 1
identity(2) // 2
identity("asdf") // "asdf"

/*
Checking identity laws
 */

val x: Int = 123
isEven(id(x)) == isEven(x) // true (for any x)
id(isEven(x)) == isEven(x) // true (for any x)

/*
Other ways of expressing composition in Scala ("compose" and "andThen")
 */

(isEven compose id[Int])(x) == (id[Int] _ andThen isEven)(x) // true (for any x)
