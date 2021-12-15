package aoc2021

import aoc2021.Day13.dots

import scala.io.Source
import scala.util.Using

object Day13 extends App {
  val input = Using(Source.fromFile("input/2021/13.txt")) {
    _.getLines.toSeq
  }.get

  val dots = input.takeWhile(_.nonEmpty).map(_.split(",").map(_.toInt)).map {case Array(x,y) => (x,y)}.toSet

  val part1 = dots.foldLeft(Set.empty[(Int,Int)])((points, p) => p match {
    case (x,y) => if (x < 655) points ++ Set((x,y)) else points ++ Set((655 - (x - 655),y))
  }).size
  println(part1)

  val folds = input.drop(dots.size + 1)

  val patternX = raw"fold along x=(\d+)".r
  val patternY = raw"fold along y=(\d+)".r

  val part2 = folds.foldLeft(dots)((dots, fold) => fold match {
    case patternX(xFold) => dots.foldLeft(Set.empty[(Int,Int)])((points, p) => p match {
      case (x,y) => if (x < xFold.toInt) points ++ Set((x,y)) else points ++ Set((xFold.toInt - (x - xFold.toInt),y))
    })
    case patternY(yFold) => dots.foldLeft(Set.empty[(Int,Int)])((points, p) => p match {
      case (x,y) => if (y < yFold.toInt) points ++ Set((x,y)) else points ++ Set((x,yFold.toInt - (y - yFold.toInt)))
    })
  })

  def foldedMaxX = part2.map(_._1).max
  def foldedMaxY = part2.map(_._2).max

  for (y <- 0 to foldedMaxY; x <- 0 to foldedMaxX) {
    if (part2.contains((x,y))) print("*") else print(".")
    if (x == foldedMaxX) println()
  }


}
