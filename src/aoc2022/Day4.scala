package aoc2022

import scala.io.Source
import scala.util.Using

object Day4 extends App {

  val input = Using(Source.fromFile("input/2022/4.txt")) {
    _.getLines().toSeq
  }.get

  val pattern = raw"(\d+)-(\d+),(\d+)-(\d+)".r

  val ranges = input.map(line =>
    pattern.findAllIn(line).subgroups.map(_.toInt) match {
      case Seq(from1, to1, from2, to2) => (from1 to to1, from2 to to2)
    })

  val part1 = ranges.count {
    case (r1, r2) => r1.containsSlice(r2) || r2.containsSlice(r1)
  }
  println(part1)

  val part2 = ranges.count {
    case (r1, r2) => r1.toSet.intersect(r2.toSet).nonEmpty
  }
  println(part2)
}

