package aoc2021

import scala.io.Source
import scala.util.Using

object Day2 extends App {
  val input = Using(Source.fromFile("input/2021/2.txt")) {
    _.getLines.toSeq
  }.get

  val forward = raw"forward (\d+)".r
  val up = raw"up (\d+)".r
  val down = raw"down (\d+)".r

  val pos = input.foldLeft((0, 0))((p, s) => s match {
    case forward(d) => (p._1 + d.toInt, p._2)
    case up(d) => (p._1, p._2 - d.toInt)
    case down(d) => (p._1, p._2 + d.toInt)
  })

  val part1 = pos._1 * pos._2
  println(part1)

  val pos2 = input.foldLeft((0, 0, 0))((p, s) =>
    s match {
      case forward(d) => (p._1 + d.toInt, p._2 + p._3 * d.toInt, p._3)
      case up(d) => (p._1, p._2, p._3 - d.toInt)
      case down(d) => (p._1, p._2, p._3 + d.toInt)
    }
  )

  val part2 = pos2._1 * pos2._2
  println(part2)

}
