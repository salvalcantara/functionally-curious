package chapter0_intro

import scala.io.StdIn.readLine

/*
Example 0.0.3 (Greeting App)
 */

object Ex003 extends App {
    println("Please, tell me your name:")
    val name = readLine()
    println(s"Hello $name")
}

/*
Minimalistic example of (side-effectful) imperative program. The main thing here is: Because
println and readLine are non-mathematical functions, one cannot really combine (or compose) them in
a functional way. All that one can do is running (or executing) the side effects they produce. In
more complex scenarios, this means that side effects take control, making it very difficult to
reason about your program. For example, because "referential transparency" is broken, refactors are
not safe and have to be done very carefully. Testing your program becomes harder, too.
 */