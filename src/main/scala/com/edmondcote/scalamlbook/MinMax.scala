package com.edmondcote.scalamlbook

import Types._


/**
  * The MinMax class normalizes single variable observations.
  */
class MinMax[T <: AnyVal](values: XSeries[T])(implicit f: T => Double) {

  case class ScaleFactors(low: Double, high: Double, ratio: Double)

  private var scaleFactors: Option[ScaleFactors] = None

  val zero = (Double.MaxValue, -Double.MaxValue)

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

  def normalize(value: Double): Double = scaleFactors.map {
    case scale if value <= min => scale.low
    case scale if value >= max => scale.high
    case scale => (value - min) * scale.ratio + scale.low
  }.get

  def setScaleFactors(low: Double, high: Double): Option[ScaleFactors] = {
    Some(ScaleFactors(low, high, (high - low) / (max - min)))
  }
}
