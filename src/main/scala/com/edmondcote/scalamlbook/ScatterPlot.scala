package com.edmondcote.scalamlbook

import java.awt.Color

import com.edmondcote.scalamlbook.Types._
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.chart.util.ShapeUtils
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}


class ScatterPlot {
  /**
    * Displays XVSeries[Double] (i.e., Vector[Array[Double]) in a scatter plot.
    */
  def display(xy: XVSeries[Double], title: String, xAxisLabel: String, yAxisLabel: String): Unit = {
    val series: XYSeries = xy.foldLeft(new XYSeries("scatter")) {
      case (s: XYSeries, x: Array[Double]) => s.add(x(0), x(1)); s
    }

    val seriesCollection = new XYSeriesCollection
    seriesCollection.addSeries(series)
    draw(seriesCollection, title, xAxisLabel, yAxisLabel)
  }

  /**
    * Displays an array of Double values in a scatter plot with counts [0, n] on X-axis and vector value on Y-Axis.
    */
  def display(ys: DoubleArray, title: String, xAxisLabel: String, yAxisLabel: String): Unit = {
    val series: XYSeries = ys.zipWithIndex.foldLeft(new XYSeries("series")) {
      case (s: XYSeries, (x: Double, n: Int)) => s.add(x, n); s
    }

    val seriesCollection = new XYSeriesCollection
    seriesCollection.addSeries(series)
    draw(seriesCollection, title, xAxisLabel, yAxisLabel)
  }

  private def draw(series: XYSeriesCollection, title: String, xAxisLabel: String, yAxisLabel: String): Unit = {
    val chart = ChartFactory.createScatterPlot(title, xAxisLabel, yAxisLabel, series,
      PlotOrientation.VERTICAL, false, false, false)

    val renderer = new XYDotRenderer
    renderer.setSeriesShape(0, ShapeUtils.createDiamond(4.0F))
    renderer.setSeriesPaint(0, Color.BLUE)
    renderer.setDotHeight(3)
    renderer.setDotWidth(3)

    val plot = chart.getPlot.asInstanceOf[XYPlot]
    plot.setRenderer(renderer)
    plot.setBackgroundPaint(Color.WHITE)

    val frame = new ChartFrame("Scatter Plot", chart)
    frame.setLocation(100, 100)
    frame.pack()
    frame.setVisible(true)
  }

}
