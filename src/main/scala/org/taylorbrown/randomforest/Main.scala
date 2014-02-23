package org.taylorbrown.randomforest

object Main extends App {
  val ftest = "breast-cancer-wisconsin.data.txt"

  println("working")
  val giniLearner = new GiniLearner
  val helper = GiniHelper(1, Map[Label, Int](), Split(-1, -1, 1))
  val examples = List(
    Example(List(1f),Label(0)),
    Example(List(2f),Label(0)),
    Example(List(1f),Label(0)),
    Example(List(1f),Label(1)),
    Example(List(2f),Label(1)),
    Example(List(2f),Label(1)),

    Example(List(3f),Label(1)),
    Example(List(3f),Label(1)),
    Example(List(3f),Label(1)),
    Example(List(3f),Label(1))

  )
//  val giniout = giniLearner.chooseAttr(examples)
  //, helper, Example(List(1,2f),Label(0))
//  println(giniout)
  testGini()
  testGiniSplit()
  def sq(x:Float) = x * x
  def testGini(){
    val lc = examples.filter(e => e.ex(0) < 3).map(_.label).groupBy(l=>l).map{case (k,v) => k -> v.length}
    println("lc map:", lc)
    val gini = giniLearner.calculateGini(lc,lc.values.sum)
    println("gini:", gini)
    println("gini should be:", 1 - sq(3f / 6) - sq(3f / 6))
  }
  def testGiniSplit(){
    val gini = giniLearner.giniSplit(Split(0, 2, 1), examples, 0)
    println("gini:", gini)
    println("should be: ", .3)
  }
}