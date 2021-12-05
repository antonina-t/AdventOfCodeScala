package aoc2016

import scala.io.Source
import scala.util.Using

object Day8 extends App {
  val input = Using(Source.fromFile("input/2016/8.txt")) {
    _.getLines.toSeq
  }.get

  val grid = Array.ofDim[Boolean](6, 50)

  val rect = raw"rect (\d+)x(\d+)".r
  val rotateRow = raw"rotate row y=(\d+) by (\d+)".r
  val rotateCol = raw"rotate column x=(\d+) by (\d+)".r

  input.foreach {
    case rect(widthS, heightS) =>
      val (width, height) = (widthS.toInt, heightS.toInt)
      for (x <- 0 until width; y <- 0 until height) grid(y)(x) = true
    case rotateRow(yS, stepsS) =>
      val (y, steps) = (yS.toInt, stepsS.toInt)
      val row = grid(y)
      val rotatedRow = row.drop(row.length - steps) ++ row.take(row.length - steps)
      grid(y).indices.foreach(x => grid(y)(x) = rotatedRow(x))
    case rotateCol(xS, stepsS) =>
      val (x, steps) = (xS.toInt, stepsS.toInt)
      val col = grid.indices.map(grid(_)(x))
      val rotatedCol = col.drop(col.length - steps) ++ col.take(col.length - steps)
      grid.indices.foreach(y => grid(y)(x) = rotatedCol(y))
  }

  val part1 = grid.map(_.count(v => v)).sum
  println(part1)

  val part2 = grid.map(_.map(v => if (v) "#" else ".").mkString).mkString("\n")
  println(part2)
}
