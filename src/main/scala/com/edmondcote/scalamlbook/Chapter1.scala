package com.edmondcote.scalamlbook

import scala.io.Source
import Types._
import Financials._


/**
  * Chapter 1 builds a model that can discriminate between volatile and nonvolatile trading sessions. Session volatility is defined as the relative difference between the session highest price and lower price. The total trading volume within a session constitutes the second parameter of the model. The relative price movement within a trading session (i.e., closing price/open price -1) is out expected values or labels.
  */
object Chapter1 extends Enumeration {
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
      .map(x => (1.0 - x(0) / x(1), x(2)))
      .toVector
      .unzip

    val volatility: DoubleArray = {
      val mM = new MinMax[Double](reducedFeatureSet._1)
      mM.normalize(0.0, 1.0).toArray
    }

    val volume: DoubleArray = {
      val mM = new MinMax[Double](reducedFeatureSet._2)
      mM.normalize(0.0, 1.0).toArray
    }

    volatility.zip(volume).map(x => Array(x._1, x._2)).toVector
  }

  def display(xv: XVSeries[Double]): Unit = {
    val plot = new ScatterPlot()
    plot.display(xv, "Relative volatility vs. Volume", "Relative stock volatility", "Daily trading volume")
  }

}
