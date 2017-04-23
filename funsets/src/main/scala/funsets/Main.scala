package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val a = union(union(singletonSet(1), singletonSet(2)), singletonSet(3))
  printSet(a)
  def p1 = (element: Int) => element+20
  val m1 = map(a, p1)
  printSet(m1)
}
