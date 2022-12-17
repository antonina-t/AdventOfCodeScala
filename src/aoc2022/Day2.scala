package aoc2022

import scala.io.Source
import scala.util.Using

object Day2 extends App {

  val input = Using(Source.fromFile("input/2022/2.txt")) {
    _.getLines().toSeq
  }.get

  val m1 = Map(
    "A X" -> 4, // 1 + 3
    "A Y" -> 8, // 2 + 6
    "A Z" -> 3, // 3 + 0
    "B X" -> 1, // 1 + 0
    "B Y" -> 5, // 2 + 3
    "B Z" -> 9, // 3 + 6
    "C X" -> 7, // 1 + 6
    "C Y" -> 2, // 2 + 0
    "C Z" -> 6) // 3 + 3

  val part1 = input.map(m1).sum
  println(part1)

  val m2 = Map(
    "A X" -> "A Z",
    "A Y" -> "A X",
    "A Z" -> "A Y",
    "B X" -> "B X",
    "B Y" -> "B Y",
    "B Z" -> "B Z",
    "C X" -> "C Y",
    "C Y" -> "C Z",
    "C Z" -> "C X")

  val part2 = input.map(m2).map(m1).sum
  println(part2)

}

