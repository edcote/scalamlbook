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
    val fieldsToDoubleArray: Fields => DoubleArray = Financials.toDoubleArray(features)

    // Reduce feature set to two variables:
    // - Relative volatility of the stock price in a session: 1.0 - LOW/HIGH
    // - Trading volume for the stock in this session: VOLUME
    val reducedFeatureSet = cols
      .map(fieldsToDoubleArray)
      .map(x => (x(1) - x(0), x(2)))
      .toVector
      .unzip


    val volatility: DoubleArray = {
      val mM = new MinMax[Double](reducedFeatureSet._1)
      println(mM.min, mM.max)
      mM.normalize(0.0, 1.0).toArray
    }

    val volume: DoubleArray = {
      val mM = new MinMax[Double](reducedFeatureSet._2)
      println(mM.min, mM.max)
      mM.normalize(0.0, 1.0).toArray
    }

    volatility.zip(volume).map(x => Array(x._1, x._2)).toVector
  }

  def main(args: Array[String]): Unit = {
    val data: XVSeries[Double] = load("CSCO.csv")
    val plot = new ScatterPlot()
    plot.display(data)
  }

}
