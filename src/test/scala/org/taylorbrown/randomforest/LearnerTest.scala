package org.taylorbrown.randomforest

import org.scalatest._

/**
 * Created by taylor on 2/22/14.
 */
class LearnerTest extends FlatSpec with Matchers{
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
  val giniLearner = new GiniLearner

  def sq(x:Float) = x * x

  "The gini value for the test set" should "be 0.5" in {
    val lc = examples.filter(e => e.ex(0) < 3).map(_.label).groupBy(l=>l).map{case (k,v) => k -> v.length}
    val gini = giniLearner.calculateGini(lc,lc.values.sum)
    gini should be (0.5f)
  }
  "The gini value for all the splits" should "be 0.3" in {
    val gini = giniLearner.giniSplit(Split(0, 2, 1), examples, 0)
    gini should be (0.3f)
  }
  "The tree learner" should "learn a tree" in {
    val tree = giniLearner.fit(examples)
    println(tree)
  }

}
