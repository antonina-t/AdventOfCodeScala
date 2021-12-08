package aoc2021

import scala.io.Source
import scala.util.Using

object Day7 extends App {
  val input = Using(Source.fromFile("input/2021/7.txt")) {
    _.getLines.toSeq
  }.get.head.split(",").map(_.toInt).toSeq

  val part1 = input.map(x0 => input.map(x1 => Math.abs(x1 - x0)).sum).min
  println(part1)

  val part2 = input.map(x0 => input.map(x1 => (1 to Math.abs(x1 - x0)).sum).sum).min
  println(part2)

}
