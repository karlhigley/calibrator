package com.github.karlhigley.calibrator

import scala.collection.mutable.{ListBuffer, Map => MutableMap}


case class Item(id: String, score: Double, attributes: Map[String, DiscreteDistribution])
case class CalibrationTerm(attribute: String, weight: Double, target: DiscreteDistribution)

class Calibration(terms: CalibrationTerm*) {
  val scoreWeight: Double = 1 - terms.map(_.weight).sum

  def rerank(items: Seq[Item]): Seq[Item] = {
    var reranked: ListBuffer[Item] = ListBuffer()
    var remaining: ListBuffer[Item] = ListBuffer()
    remaining ++= items

    val previousItemAttrs: MutableMap[String, DiscreteDistribution] =
      MutableMap(terms.map(_.attribute).map((_, DiscreteDistribution.empty)): _*)

    while (remaining.size > 0) {
      val nextItem = greedySelection(remaining, scoreWeight, previousItemAttrs, terms)
      reranked += nextItem
      remaining -= nextItem

      // Update attribute distributions
      for (term <- terms) {
        previousItemAttrs(term.attribute) = previousItemAttrs(term.attribute) + nextItem.attributes(term.attribute)
      }
    }

    reranked
  }

  private def greedySelection(items: Seq[Item],
                              scoreWeight: Double,
                              previousItemAttrs: MutableMap[String, DiscreteDistribution],
                              terms: Seq[CalibrationTerm],
                              ): Item = {
    // Select the item with the highest marginal relevance
    items.map(item => {
      (item, marginalRelevance(item, scoreWeight, previousItemAttrs, terms))
    }).maxBy(_._2)._1
  }

  private def marginalRelevance(item: Item,
                                scoreWeight: Double,
                                previousItemAttrs: MutableMap[String, DiscreteDistribution],
                                terms: Seq[CalibrationTerm],
                                ): Double = {
    val weightedScore = item.score * scoreWeight
    val combinedTerms = terms.map(t => {
      val distSoFar = previousItemAttrs(t.attribute)
      val termResult = t.weight * safeKlDivergence(t.target, (distSoFar + item.attributes(t.attribute)).normalize)
      termResult
    }).sum
    weightedScore - combinedTerms
  }

  private def safeKlDivergence(p: DiscreteDistribution, q: DiscreteDistribution, weight: Double = 0.01): Double = {
    val adjustedQ = q * (1 - weight) + (p * weight)
    p.klDivergence(adjustedQ)
  }
}

object Calibration {
  def single(attribute: String, weight: Double, target: DiscreteDistribution): Calibration = {
    new Calibration(CalibrationTerm(attribute, weight, target))
  }

  def single(attribute: String, weight: Double, target: Map[String, Double]): Calibration = {
    new Calibration(CalibrationTerm(attribute, weight, DiscreteDistribution(target)))
  }
}


