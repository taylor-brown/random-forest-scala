package org.taylorbrown.randomforest

import scala.util.Random

case class Label(label: Int)

case class Split(attr: Int, split: Float, measure: Float)
abstract class Node(split: Split, lc: Option[Node], rc: Option[Node])
case class TreeNode(split: Split, lc: Node, rc: Node) extends Node(split, Option(lc), Option(rc))
case class Leaf(label: Label = Label(0)) extends Node(Split(0, 0, 1), None, None)
case class Example(ex: Seq[Float], label: Label)

trait TreeLearner {
  def chooseAttr(attr: Seq[Example]): Split

  def fit(ex: Seq[Example], depthLeft:Int = 15): Node = {
    if (ex.length < 1) return Leaf() //shouldn't happen?
    if (ex.map(_.label).toSet.size == 1) return Leaf(ex.head.label) // all same label
//    if (ex.takeWhile(_.ex.equals(ex.head.ex)).length == ex.length) return Leaf(ex.map(_.label).groupBy(l=>l).maxBy(_._2.length)._1) // all same values
    if(depthLeft < 1) return Leaf(ex.map(_.label).groupBy(l=>l).maxBy(_._2.length)._1) // return max class
    val inx = chooseAttr(ex)
    val (lc, rc) = ex.partition(ex => ex.ex(inx.attr) <= inx.split)
    TreeNode(inx, fit(lc, depthLeft-1), fit(rc, depthLeft -1 ))
  }

  def classify(ex: Example, tree: Node): Label = {
    tree match {
      case leaf: Leaf => leaf.label
      case node: TreeNode => if (ex.ex(node.split.attr) <= node.split.split) {
        classify(ex, node.lc)
      } else classify(ex, node.rc)
    }
  }
}

abstract class BaseGiniLearner extends TreeLearner {
  def sq(x: Float) = x * x
  def chooseAttr(attr: Seq[Example]): Split = {
    val ginis = for (i <- getAttributes(attr)) yield {
      //todo - distinct value calcs
      attr.foldLeft(Split(i, attr.head.ex(i), 1)){case (curSplit, e) =>
        val newgini = giniSplit(Split(i, e.ex(i), 1), attr, i)
        if(newgini < curSplit.measure){
          Split(i, e.ex(i), newgini)//todo - split between values
        }else curSplit
      }
    }
    ginis.minBy(f => f.measure)
  }


  def getAttributes(attr: Seq[Example]):Seq[Int]

  def giniSplit(split:Split, attrs:Seq[Example], attr:Int)= {
    val totalCount = attrs.length
    val (ltLabels, gtLabels) = attrs.partition(ex => ex.ex(attr) <= split.split)
    def getCounts(label:Seq[Label])= {
      val labelCounts = label.groupBy(l => l).map{case (k,v) => k -> v.length}
      calculateGini(labelCounts, label.length)
    }
    val ltg = ltLabels.length.toFloat/totalCount * getCounts(ltLabels.map(_.label))
    val gtg = gtLabels.length.toFloat/totalCount* getCounts(gtLabels.map(_.label))
    ltg + gtg
  }
  def calculateGini(labelCounts:Map[Label,Int], totalCount:Int) = {
    1 - labelCounts.map{case (k,v) => sq(v.toFloat/totalCount)}.sum
  }
}

class GiniLearner extends BaseGiniLearner {
  def getAttributes(attr: Seq[Example]) = {
    0 to attr.head.ex.length - 1
  }
}

  class RandomGiniLearner extends BaseGiniLearner {
    def getAttributes(attr: Seq[Example]) = {
      val num = math.max(math.sqrt(attr.head.ex.length), 1).toInt // use sqrt of number of attributes
      Random.shuffle((0 to (attr.head.ex.length -1)).toList).take(num)
    }

  }

