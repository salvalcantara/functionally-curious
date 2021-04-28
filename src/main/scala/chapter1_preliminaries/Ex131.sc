/*
Example 1.3.1
 */

val add: (Int, Int) => Int = (a, b) => a + b
val lazyAdd: Int => Int => Int = a => b => a + b

/*
Or, if you prefer:
  def add(a: Int, b: Int): Int = a + b
  def lazyAdd(a: Int)(b: Int): Int = a + b // Has 2 parameter lists
 */

val add1: Int => Int = lazyAdd(1) // Increment by 1: add1(n) is n + 1

add1(0) // 1
add1(1) // 2

/*
The following functions prove that
  "(A, B) => C is isomorphic to A => B => C"
 */

def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)
def uncurry[A, B, C](f: A => B => C): (A, B) => C =(a, b) => f(a)(b)

/*
We get to lazyAdd by currying add
 */

val curriedAdd = curry(add) // Same as lazyAdd
val plus1: Int => Int = curriedAdd(1) // Same as add1

plus1(0) // 1
plus1(1) // 2
