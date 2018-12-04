package com.edmondcote.scalamlbook

import scala.io.Source
import Types._
import Financials._


object StockPriceModel extends Enumeration {
  def load(resourceName: String): Unit = {
    val src = Source.fromResource(resourceName)
    val data = extract(src.getLines().map(_.split(",")).drop(1))
    src.close()
  }

  def extract(cols: Iterator[Fields]): XVSeries[Double] = {
    val features = Array[Financials](LOW, HIGH, VOLUME)
    val convert: Fields => Array[Double] = Financials.toDoubleArray(features)

    cols
      .map(convert).toVector
      .map(x => Array[Double](1.0 - x(0) / x(1), x(2)))
  }

  def main(args: Array[String]): Unit = {
    load("CSCO.csv")
  }
}
