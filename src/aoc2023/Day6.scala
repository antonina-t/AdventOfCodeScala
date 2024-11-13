package aoc2023

import scala.io.Source
import scala.util.Using

object Day6 extends App {

  val input = Using(Source.fromFile("input/2023/6.txt")) {
    _.getLines().toSeq
  }.get

  val times = Array(38, 94, 79, 70)
  val distances = Array(241, 1549, 1074, 1091)

  val part1 = times.indices.map(i => {
    (1 until times(i)).count(holdButton => (times(i) - holdButton) * holdButton > distances(i))
  }).product

  println(part1)

  val time = 38947970
  val distance = 241154910741091L

  // (time - x) * x > distance
  val x1 = 7723190
  val x2 = 31224779

  val part2 = x2 - x1
  println(part2)
}

