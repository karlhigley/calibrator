package com.github.karlhigley.calibrator

class DiscreteDistribution(val masses: Map[String, Double] = Map()) {

  def apply(key: String) = masses.getOrElse(key, 0.0)

  def +(q: DiscreteDistribution): DiscreteDistribution = {
    elementwise(q, (a: Double, b: Double) => a + b)
  }

  def *(q: DiscreteDistribution): DiscreteDistribution = {
    elementwise(q, (a: Double, b: Double) => a * b)
  }

  def *(scalar: Double): DiscreteDistribution = {
    val scaled: Map[String, Double] = masses.mapValues { mass => scalar * mass }
    new DiscreteDistribution(scaled)
  }

  def normalize: DiscreteDistribution = {
    this * (1/masses.values.sum)
  }

  def klDivergence(q: DiscreteDistribution): Double = {
    // By convention, 0 * ln(0/a) = 0, so we can ignore keys in q that aren't in p
    val addends: Iterable[Double] = masses.keys.map { key: String => {
        this(key) * Math.log(this(key) / q(key))
      }
    }
    addends.reduce(_+_)
  }

  private def elementwise(q: DiscreteDistribution, f: (Double, Double) => Double): DiscreteDistribution = {
    val keys: Set[String] = (masses.keys ++ q.masses.keys).toSet
    val combined: Set[(String, Double)] = keys.map(key => {
      (key, f(this(key), q(key)))
    })
    new DiscreteDistribution(combined.toMap)
  }
}

object DiscreteDistribution {
  def apply(elements: Map[String, Double]): DiscreteDistribution = {
    new DiscreteDistribution(elements)
  }

  def apply(elements: (String, Double)*): DiscreteDistribution = {
    new DiscreteDistribution(elements.toMap)
  }

  def empty: DiscreteDistribution = {
    DiscreteDistribution()
  }

  def degenerate(key: String): DiscreteDistribution = {
    DiscreteDistribution((key, 1.0))
  }

  def uniform(keys: String*): DiscreteDistribution = {
    val mass = 1.0/keys.size
    DiscreteDistribution(keys.map((_, mass)).toMap)
  }

  def bernoulli(prob: Double, positive: String, negative: String): DiscreteDistribution = {
    DiscreteDistribution((positive, prob), (negative, 1-prob))
  }
}