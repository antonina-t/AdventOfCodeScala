package aoc2021

import scala.io.Source
import scala.util.Using

object Day5 extends App {
  val input = Using(Source.fromFile("input/2021/5.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = raw"(\d+),(\d+) -> (\d+),(\d+)".r

  def intersections(includeDiagonal: Boolean) = {
    val grid = Array.ofDim[Int](1000, 1000)
    input.foreach {
      case pattern(x1s, y1s, x2s, y2s) => {
        val x1 = x1s.toInt
        val x2 = x2s.toInt
        val y1 = y1s.toInt
        val y2 = y2s.toInt
        val stepX = if (x1 < x2) 1 else -1
        val stepY = if (y1 < y2) 1 else -1
        val line =
          if (x1 == x2)
            (y1 to y2 by stepY).map((x1, _))
          else if (y1 == y2)
            (x1 to x2 by stepX).map((_, y1))
          else if (includeDiagonal)
            (x1 to x2 by stepX) zip (y1 to y2 by stepY)
          else Seq.empty
        line.foreach {
          case (x, y) => grid(y)(x) = grid(y)(x) + 1
        }
      }
    }
    grid.map(_.count(_ > 1)).sum
  }

  val part1 = intersections(false)
  println(part1)

  val part2 = intersections(true)
  println(part2)

}
