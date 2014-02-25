package org.taylorbrown.randomforest

object Main extends App {
  val ftest = "breast-cancer-wisconsin.data.txt"
  readLine()
  val lines = scala.io.Source.fromFile(ftest).mkString.split("\n")
  val exs = for(line <- lines) yield{
    val cols = line.split(",")
    val attrs = cols.reverse.tail.reverse.map(_.toFloat)
    val label = Label(cols.last.toInt)
    Example(attrs, label)
  }
  println("training...")
  val forest = new Forest(exs, nTrees = 100)
  println("classifying...")
  println((for (e <- exs) yield forest.classify(List(e)).head == e).count(e=>e))
}