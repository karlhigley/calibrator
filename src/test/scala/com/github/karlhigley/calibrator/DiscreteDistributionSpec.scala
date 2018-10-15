package com.github.karlhigley.calibrator

import org.scalatest.{FlatSpec, Matchers}

class DiscreteDistributionSpec extends FlatSpec with Matchers {
  val distA = DiscreteDistribution(("a", 0.2), ("b", 0.5), ("c", 0.3))
  val distB = DiscreteDistribution(("b", 0.3), ("c", 0.5), ("d", 0.2))

  "apply" should "retrieve the element for a particular key" in {
    val result = distA("a")
    val expected = 0.2
    result should be (expected)
  }

  "+" should "element-wise add two distributions" in {
    val result = (distA + distB).masses
    val expected = Map(("a", 0.2), ("b", 0.8), ("c", 0.8), ("d", 0.2))
    result should be (expected)
  }

  "*" should "element-wise multiply distribution by a scalar" in {
    val result = (distA * 0.5).masses
    val expected = Map(("a", 0.1), ("b", 0.25), ("c", 0.15))
    result should be (expected)
  }

  "*" should "element-wise multiply distribution by another distribution" in {
    val result = (distA * distB).masses
    val expected = Map(("a", 0.0), ("b", 0.15), ("c", 0.15), ("d", 0.0))
    result should be (expected)
  }

  "klDivergence" should "be zero when distributions are the same" in {
    val result = distA.klDivergence(distA)
    val expected = 0.0
    result should be (expected)
  }

  "klDivergence" should "compute divergence between this and target distribution" in {
    val p = new DiscreteDistribution(Map(("a", 0.2), ("b", 0.5), ("c", 0.3)))
    val q = new DiscreteDistribution(Map(("a", 0.4), ("b", 0.5), ("c", 0.1)))
    /*
    * 0.2 * log (0.5) + 0.5 * log (1) + 0.3 * log (3)
    * -0.138629436111989 + 0 + 0.329583686600433 = 0.1909542504884438
    */
    val result = p.klDivergence(q)
    val expected = 0.1909542504884438
    result should be (expected)
  }
}
