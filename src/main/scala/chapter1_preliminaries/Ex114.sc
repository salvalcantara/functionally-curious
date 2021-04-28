/*
Example 1.1.4
 */

def absurd[A]: Nothing => A = ??? // No actual body required!

/*
This function is called absurd because it cannot be called. It models an impossible computation, if
you like. This is because there is no way to pass it a value of type Nothing, since there is none.
 */

def unit[A]: A => Unit = _ => () // _ means "whatever"

/*
This function, unit, returns () for whatever input value we supply.
 */

unit(1) // ()
unit("asdf") // ()


