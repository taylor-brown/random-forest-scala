package org.taylorbrown.randomforest

case class Label(label: Int)

case class Split(attr: Int, split: Float, measure: Float)
abstract class Node(split: Split, lc: Option[Node], rc: Option[Node])
case class TreeNode(split: Split, lc: Node, rc: Node) extends Node(split, Option(lc), Option(rc))
case class Leaf(label: Label = Label(0)) extends Node(Split(0, 0, 1), None, None)
case class Example(ex: Seq[Float], label: Label)

trait TreeLearner {
  def chooseAttr(attr: Seq[Example]): Split

  def fit(ex: Seq[Example]): Node = {
    if (ex.length < 1) return Leaf() //shouldn't happen?
    if (ex.map(_.label).toSet.size == 1) return Leaf(ex.head.label)
    val inx = chooseAttr(ex)
    val (lc, rc) = ex.partition(ex => ex.ex(inx.attr) <= inx.split)
    TreeNode(inx, fit(lc), fit(rc))
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

case class GiniHelper(gini: Float, labelCounts: Map[Label, Int], split: Split)

class GiniLearner extends TreeLearner {
  def sq(x: Float) = x * x
  def chooseAttr(attr: Seq[Example]): Split = {
    val ginis = for (i <- 0 to attr.head.ex.length-1) yield {

      attr.foldLeft(Split(i, attr.head.ex(i), 1)){case (curSplit, e) =>
        val newgini = giniSplit(Split(i, e.ex(i), 1), attr, i)
        if(newgini < curSplit.measure){
          Split(i, e.ex(i), newgini)//todo - split between values
        }else curSplit
      }
//      vals.view.zipWithIndex.foldLeft(GiniHelper(1, Map[Label, Int](), Split(i, attrs.last.ex(i)))) ({
//        case (gini: GiniHelper, (ex: Example, index)) => {
//          val (newmap, newgini) = calculateGinis(attrs, gini, ex)
//          if (newgini < gini.gini) GiniHelper(newgini, newmap, Split(i, ex.ex(i))) else GiniHelper(gini.gini, newmap, gini.split)
//        }
//      })
    }
    ginis.minBy(f => f.measure)
  }
  
//  def calculateGinis(attrs: Seq[org.taylorbrown.randomforest.Example], gini: GiniHelper, ex: org.taylorbrown.randomforest.Example): (scala.collection.immutable.Map[org.taylorbrown.randomforest.Label,Int], Int) = {
//    val newmap = gini.labelCounts + (ex.label -> (gini.labelCounts.getOrElse(ex.label, 0) + 1))
//    val newgini = (for (label <- newmap.keys) yield {
//      val ltfreqs = for (cl <- newmap.keys) yield {
//        attrs.length - newmap(cl)
//      }
//      val gtfreqs = for (cl <- newmap.keys) yield {
//        newmap(cl)
//      }
//      newmap(label) / attrs.length * (1 - sq(ltfreqs.map(_ / newmap(label)).sum) + sq(gtfreqs.map(_ / newmap(label)).sum))
//    }).sum
//    (newmap, newgini)
//  }

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
