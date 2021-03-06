package com.edmondcote.scalamlbook

import Types._


/**
  * The Financials object models the end-of-day stock price.
  */
object Financials extends Enumeration {
  type Financials = Value

  val DATE, OPEN, HIGH, LOW, CLOSE, VOLUME, ADJ_CLOSE = Value

  def toDouble(v: Value): Fields => Double = (s: Fields) => s(v.id).toDouble

  def toDoubleArray(vs: Array[Value]): Fields => Array[Double] = (s: Fields) => vs.map(v => s(v.id).toDouble)
}
