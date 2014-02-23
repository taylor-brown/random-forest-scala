package org.taylorbrown.randomforest

import org.scalatest._

/**
 * Created by taylor on 2/23/14.
 */
class ForestTest extends FlatSpec with Matchers {
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

  "The forest " should "classify things mostly right" in {
    val forest = new Forest(examples, nTrees = 100)
//    println(forest.probs(examples))
    println(forest.classify(examples))
    (for (e <- examples) yield forest.classify(List(e)).head == e.label).count(e => e) should be > 7
  }
}
