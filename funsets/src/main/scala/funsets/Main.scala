package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val s1 = singletonSet(1)
  printSet(s1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s = union(s1, s2)
  val ss = union(s, s3)
  printSet(ss)
  val newS = map(ss, _*5)
  printSet(newS)
}
