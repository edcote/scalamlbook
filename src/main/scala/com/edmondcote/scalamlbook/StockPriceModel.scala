package com.edmondcote.scalamlbook

import scala.io.Source
import Types._
import Financials._


/**
  * The StockPriceModel object contains main function.
  */
object StockPriceModel extends Enumeration {
  def load(resourceName: String): XVSeries[Double] = {
    val src = Source.fromResource(resourceName)
    val lines = src.getLines()
    val fields: Iterator[Fields] = lines.map(_.split(","))
    val cols = fields.drop(1)
    val data = extract(cols)
    src.close()
    data
  }

  def extract(cols: Iterator[Fields]): XVSeries[Double] = {
    val features = Array[Financials](LOW, HIGH, VOLUME)
    val convert: Fields => Array[Double] = Financials.toDoubleArray(features)

    cols
      .map(convert).toVector
      .map(x => Array[Double](1.0 - x(0) / x(1), x(2)))
  }

  def main(args: Array[String]): Unit = {
    val data = load("CSCO.csv")
    data.take(10).foreach(x => println(x.mkString(", ")))
  }
}
