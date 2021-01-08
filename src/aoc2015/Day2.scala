package aoc2015

import scala.io.Source
import scala.util.Using

object Day2 extends App {
  val input = Using(Source.fromFile("input/2015/2.txt")) {
    _.getLines.toSeq
  }.get

  val dims = input
    .map(_.split("x").map(_.toInt))

  val part1 = dims
    .map {
      case Array(a, b, c) => Array(a * b, b * c, a * c)
    }
    .map(sides => sides.min + sides.sum * 2)
    .sum
  println(part1)

  val part2 = dims
    .map(sides => sides.sorted.take(2).sum * 2 + sides.product)
    .sum
  println(part2)
}
