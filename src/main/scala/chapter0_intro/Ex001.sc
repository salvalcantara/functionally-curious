/*
Example 0.0.1
 */

val x: Vector[Float] = Vector(1, 2, 3)
val y: Vector[Float] = Vector(4, 5, 6)

// Imperative solution
var sum: Float = 0
for (i <- 0 until x.size)
  sum += x(i) * y(i)
sum

// Functional solution
x.lazyZip(y).map(_ * _).reduce(_ + _)
