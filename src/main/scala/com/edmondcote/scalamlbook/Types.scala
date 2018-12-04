package com.edmondcote.scalamlbook

/**
  * The Types object contains user defined types.
  */
object Types {
  type Fields = Array[String]
  type XSeries[T] = Vector[T] // time series of parameterized type T
  type XVSeries[T] = Vector[Array[T]]
}
