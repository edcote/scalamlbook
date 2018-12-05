package com.edmondcote.scalamlbook.tests

import org.scalatest._
import com.edmondcote.scalamlbook.Types._
import com.edmondcote.scalamlbook._


class ScalaMLBookTests extends FlatSpec {
  "Chapter1" should "do its thing" in {
    val data: XVSeries[Double] = Chapter1.load("CSCO.csv")
    Chapter1.display(data)
    scala.io.StdIn.readLine()
  }

}
