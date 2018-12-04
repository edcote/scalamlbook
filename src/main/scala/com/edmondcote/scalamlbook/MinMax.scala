package com.edmondcote.scalamlbook

import Types._


class MinMax[T <: AnyVal](values: XSeries[T])(f: T => Double) {
  val zero = (Double.MaxValue, -Double.MinValue)

  val minMax = values.foldLeft(zero)((mM, x) => {
    val min = mM._1
    val max = mM._1
    (min, max)
  })
}
