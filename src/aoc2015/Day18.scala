package aoc2015

import scala.io.Source
import scala.util.Using

object Day18 extends App {
  val input = Using(Source.fromFile("input/2015/18.txt")) {
    _.getLines.toSeq
  }.get

  val grid = input
    .zipWithIndex
    .flatMap { case (row, i) => row
      .zipWithIndex
      .filter(_._1 == '#')
      .map { case (_, j) => (i, j) }
    }
    .toSet

  val corners = Set((0, 0), (0, 99), (99, 0), (99, 99))

  def iter(grid: Set[(Int, Int)], cornersAlwaysOn: Boolean): Set[(Int, Int)] =
    (for (x <- 0 until 100; y <- 0 until 100) yield {
      val neighbours = (
        for (dx <- -1 to 1; dy <- -1 to 1 if (dx, dy) != (0, 0))
          yield if (grid.contains((x + dx, y + dy))) 1 else 0
        ).sum
      if ((cornersAlwaysOn && corners.contains((x, y))) ||
        (grid.contains((x, y)) && (neighbours == 2 || neighbours == 3)) ||
        (!grid.contains((x, y)) && neighbours == 3))
        Some((x, y))
      else
        None
    }).flatten.toSet

  val part1 = (1 to 100)
    .foldLeft(grid)((acc, _) => iter(acc, cornersAlwaysOn = false)).size
  println(part1)

  val part2 = (1 to 100)
    .foldLeft(grid ++ corners)((acc, _) => iter(acc, cornersAlwaysOn = true)).size
  println(part2)
}
