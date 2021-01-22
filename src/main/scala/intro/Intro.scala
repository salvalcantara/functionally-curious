package intro

object Intro {

  import cats._
  import cats.implicits._

  Foldable[List].fold(List("a", "b", "c"))
  // res0: String = "abc"
  Foldable[List].foldMap(List(1, 2, 4))(_.toString)
  // res1: String = "124"
  Foldable[List].foldK(List(List(1,2,3), List(2,3,4)))
  // res2: List[Int] = List(1, 2, 3, 2, 3, 4)
  Foldable[List].reduceLeftToOption(List[Int]())(_.toString)((s,i) => s + i)
  // res3: Option[String] = None
  Foldable[List].reduceLeftToOption(List(1,2,3,4))(_.toString)((s,i) => s + i)
  // res4: Option[String] = Some("1234")
  Foldable[List].reduceRightToOption(List(1,2,3,4))(_.toString)((i,s) => Later(s.value + i)).value
  // res5: Option[String] = Some("4321")
  Foldable[List].reduceRightToOption(List[Int]())(_.toString)((i,s) => Later(s.value + i)).value

  def main(args: Array[String]): Unit = {
    println("Functionally curious")
  }

}
