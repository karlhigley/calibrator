package com.github.karlhigley.calibrator

import org.scalatest.{FlatSpec, Matchers}

class CalibrationSpec extends FlatSpec with Matchers {

  val genreTarget = DiscreteDistribution(("drama", 0.4), ("comedy", 0.3), ("romance", 0.2), ("documentary", 0.1))
  val terms = List(CalibrationTerm("genre", 0.2, genreTarget))
  val calibration = new Calibration(terms: _*)

  val genresA = DiscreteDistribution(("drama", 1.0))
  val itemA = Item("itemA", 0.8, Map(("genre", genresA)))

  val genresB = DiscreteDistribution(("drama", 0.5), ("comedy", 0.5))
  val itemB = Item("itemB", 0.7, Map(("genre", genresB)))

  val genresC = DiscreteDistribution(("comedy", 0.5), ("action", 0.5))
  val itemC = Item("itemC", 0.5, Map(("genre", genresC)))

  val genresD = DiscreteDistribution(("documentary", 1.0))
  val itemD = Item("itemD", 0.4, Map(("genre", genresD)))

  val genresE = DiscreteDistribution(("drama", 0.5), ("romance", 0.5))
  val itemE = Item("itemE", 0.4, Map(("genre", genresE)))

  "rerank" should "return the original number of items" in {
    val items = List(itemA, itemB, itemC, itemD, itemE)
    val result = calibration.rerank(items)
    result.size should be (items.size)
  }

  "rerank" should "choose a slightly lower scoring item with a better distribution match" in {
    val items = List(itemA, itemB)
    val result = calibration.rerank(items)
    val order = result.map(_.id)
    order should be (List("itemB", "itemA"))
  }

  "rerank" should "resolve scoring ties by choosing item with better distribution match" in {
    val items = List(itemD, itemE)
    val result = calibration.rerank(items)
    val order = result.map(_.id)
    order should be (List("itemE", "itemD"))
  }
}
