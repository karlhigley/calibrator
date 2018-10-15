package com.github.karlhigley.calibrator

object Implicits {
  implicit def map2distribution(x: Map[String, Double]): DiscreteDistribution = {
    new DiscreteDistribution(x)
  }
}
