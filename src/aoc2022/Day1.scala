package aoc2022

import scala.io.Source
import scala.util.Using

object Day1 extends App {

  val input = Using(Source.fromFile("input/2022/1.txt")) {
    _.mkString
  }.get

  val calsPerElf = input.split("\n\n").map(_.split("\n").map(_.toInt).sum)

  val part1 = calsPerElf.max
  println(part1)

  val part2 = calsPerElf.sorted.reverse.take(3).sum
  println(part2)

  println(Integer.MAX_VALUE + 1)
}

