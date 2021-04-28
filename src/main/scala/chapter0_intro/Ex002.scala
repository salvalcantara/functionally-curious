package chapter0_intro

import scala.io.StdIn.readLine

/*
Example 0.0.2
 */

object Ex002 extends App {

    /*
    Consider the following type signatures:

      - def println(x: Any): Unit
      - def readLine(): String

    These are examples of non-mathematical (I/O) functions. As a reminder, mathematical
    functions are:

      1. Total (defined for all inputs)
      2. Deterministic (given the same input, return the same output)
      3. Pure (produce no "side effects")
     */

    println("Please, tell me your name:")

    /*
    This returns () (essentially nothing) in a deterministic way and for any given input. So, println
    is total and deterministic. However, other than returning () produces the side effect of printing
    the given message to the screen, so it's not pure because it has an observable aside, so to speak.
     */

    val name = readLine()

    /*
    In this occasion, readLine is trivially total (since it receives no inputs at all!) but it's not
    deterministic. The output (name) depends on the effect of reading the input from the user, so it
    might be different each time.
     */
}
