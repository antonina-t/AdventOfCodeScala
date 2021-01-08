package aoc2015

import scala.io.Source
import scala.util.Using

object Day1 extends App {
  val input = Using(Source.fromFile("input/2015/1.txt")) {
    _.mkString
  }.get

  val part1 = input.count(_ == '(') - input.count(_ == ')')
  println(part1)

  val part2 = input
    .scanLeft(0)((floor, c) => if (c == '(') floor + 1 else floor - 1)
    .indexOf(-1)
  println(part2)
}
