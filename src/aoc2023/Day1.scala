package aoc2023

import scala.io.Source
import scala.util.Using

object Day1 extends App {

  val input = Using(Source.fromFile("input/2023/1.txt")) {
    _.getLines().toSeq
  }.get

  val part1 = input.map(s => s.find(_.isDigit).get.toString ++ s.findLast(_.isDigit).get.toString).map(_.toInt).sum
  println(part1)

  val words = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val digits = (0 to 9).map(_.toString) ++ words
  val digitMap = digits.zip((0 to 9) ++ (1 to 9)).toMap
  val part2 = input.map(s => {
    val left = s.indices.flatMap(i => digits.find(d => s.substring(i).startsWith(d))).head
    val right = s.indices.flatMap(i => digits.find(d => s.substring(0, s.length - i).endsWith(d))).head
    digitMap(left) * 10 + digitMap(right)
  }).sum

  println(part2)
}

