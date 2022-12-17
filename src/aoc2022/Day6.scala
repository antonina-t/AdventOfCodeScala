package aoc2022

import scala.io.Source
import scala.util.Using

object Day6 extends App {

  val input = Using(Source.fromFile("input/2022/6.txt")) {
    _.getLines().mkString
  }.get

  def findMarkerOffset(size: Int) =
    input.sliding(size).zipWithIndex.find {
      case (group, _) => group.toSet.size == size
    }.map(_._2).get + size

  val part1 = findMarkerOffset(4)
  println(part1)

  val part2 = findMarkerOffset(14)
  println(part2)
}

