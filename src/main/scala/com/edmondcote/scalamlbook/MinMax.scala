package com.edmondcote.scalamlbook

import Types._


/**
  * The MinMax class normalizes single variable observations.
  */
class MinMax[T <: AnyVal](values: XSeries[T])(implicit f: T => Double) {
  val zero = (Double.MaxValue, -Double.MinValue)

  val minMax = values.foldLeft(zero)((mM, x) => {
    val min = mM._1
    val max = mM._2
    (if (x < min) x else min, if (x > max) x else max)
  })

  def min = minMax._1

  def max = minMax._2

  def normalize(low: Double, high: Double): Vector[Double] =
    setScaleFactors(low, high).map(scale => {
      values.map(x => (x - min) * scale.ratio + scale.low)
    }).getOrElse(throw new IllegalStateException("MinMax.normalize params undefined"))

  case class ScaleFactors(low: Double, high: Double, ratio: Double)

  def setScaleFactors(low: Double, high: Double): Option[ScaleFactors] = {
    Some(ScaleFactors(low, high, (high - low) / (max - min)))
  }
}
