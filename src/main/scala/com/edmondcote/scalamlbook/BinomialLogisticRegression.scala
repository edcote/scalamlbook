package com.edmondcote.scalamlbook

import Types._


/**
  * Binomial logistic regression classifier.
  *
  * @param observations  Vector of observations that represent volume and volatility
  * @param expected      Vector of expected values
  * @param maxIterations Maximum number of iterations allowed for the optimizer to extract the regression weights during training
  * @param eta           Learning (or training) weight
  * @param eps           Maximum value of the error (predicted - expected) for which the model is valid
  */
class BinomialLogisticRegression(observations: Vector[DoubleArray],
                                 expected: Vector[Int],
                                 maxIterations: Int,
                                 eta: Double,
                                 eps: Double) {

  import BinomialLogisticRegression._

  //val model = BinomialLogisticRegressionModel = train

  //def classify(observation: DoubleArray): Try[(Int, Double)]

  //def train: BinomialLogisticRegressionModel

  /** Returns value of weights when the observations have null values. */
  def intercept(weights: DoubleArray): Double = {
    val zeroObservations = observations.filter(!_.exists(_ > 0.01))
    if (zeroObservations.nonEmpty)
      zeroObservations.aggregate(0.0)((s, z) => s + dot(z, weights), _ + _) / zeroObservations.size
    else
      0.0
  }

}

object BinomialLogisticRegression {
  /**
    * Computes the dot product of observations and weights by adding the bias element (or input) to the array of observations.
    *
    * @param observations Array of observations (dimension of the model)
    * @param weights      Array (or number of features + 1) weights
    * @return w0 + w1.x1 + .. + wn.xn
    */
  def dot(observations: DoubleArray, weights: DoubleArray): Double = {

    val biasObservations: Array[Double] = Array[Double](1.0) ++ observations.view

    //def aggregate[B](z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)
    weights.zip(biasObservations).aggregate(0.0)((s, x) => s + x._1 * x._2, _ + _)
  }
}
