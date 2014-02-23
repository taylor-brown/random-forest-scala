package org.taylorbrown.randomforest

import scala.util.Random

/**
 * Created by taylor on 2/23/14.
 */
class Forest( exs:Seq[Example], nTrees:Int = 500, treeLearner:TreeLearner = new RandomGiniLearner){

  val rand = new Random()
  val trees = fit(exs)
  def sample(seq:Seq[Example]) = for(i <- 0 to seq.length) yield {
    seq(rand.nextInt(seq.length-1))
  } // generic?
  def fit(exs:Seq[Example]) = {
    for(t <- 0 to nTrees) yield{
      treeLearner.fit(sample(exs))
    }
  }

  def probs(exs:Seq[Example]) = {
    for(ex <- exs) yield{
      val labels = for(t <- trees)yield{
        treeLearner.classify(ex, t)
      }
      labels.groupBy(l=>l).map{case (k,v ) => k -> v.length}
    }
  }

  def classify(exs:Seq[Example]) = probs(exs).map(lmap => lmap.maxBy(_._2)._1)
}

