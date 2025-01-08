package aoc2024

import scala.io.Source
import scala.util.Using

object Day3 extends App {

  val input = Using(Source.fromFile("input/2024/3.txt")) {
    _.getLines().toSeq
  }.get.mkString

  val pattern = raw"mul\((-?\d+),(-?\d+)\)".r

  val part1 = pattern.findAllIn(input).map {
    case pattern(n1, n2) => n1.toInt * n2.toInt
  }.sum
  println(part1)

  val pattern2 = raw"do\(\)|don't\(\)|mul\((-?\d+),(-?\d+)\)".r

  val part2 = pattern2.findAllIn(input.mkString).foldLeft((0, true))((acc, curr) => {
    val (sum, should) = acc
    curr match {
      case "do()" => (sum, true)
      case "don't()" => (sum, false)
      case pattern(n1, n2) => (sum + (if (should) n1.toInt * n2.toInt else 0), should)
    }
  }
  )._1
  println(part2)

}
