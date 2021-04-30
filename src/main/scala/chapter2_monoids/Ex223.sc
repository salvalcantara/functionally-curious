import chapter2_monoids._
import chapter2_monoids.Ex222._

/*
Example 2.2.3
 */

/*
With Fix, we can build arbitrarily nested expressions, and all will have the same (single) type:
  - Fix[ExpressionF]
 */

val expr: Fix[ExpressionF] =
  Fix(Add(
    Fix[ExpressionF](One),
    Fix(Neg(
      Fix(Mul(
        Fix[ExpressionF](One),
        Fix[ExpressionF](One)
      ))
    ))
  ))

/*
Albeit not very practical, we can generate arbitrary elements/literals by repeatedly adding one
 */

val one:   Fix[ExpressionF] = Fix[ExpressionF](One)
val two:   Fix[ExpressionF] = Fix(Add(one, one))
val three: Fix[ExpressionF] = Fix(Add(one, two))
// ...
