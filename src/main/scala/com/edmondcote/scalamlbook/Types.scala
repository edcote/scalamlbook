package com.edmondcote.scalamlbook

/**
  * The Types object contains user defined types.
  */
object Types {
  type DoublePair = (Double, Double)
  type DoubleMatrix = Array[Array[Double]]
  type DoubleArray = Array[Double]
  type DoubleVector = Vector[Double]
  type DoublePairVector = Vector[(Double, Double)]

  type Fields = Array[String]
  type XSeries[T] = Vector[T] // time series of a single XSeries[T] variable
  type XVSeries[T] = Vector[Array[T]] // time series of multiple XSeries[T] variables
}
