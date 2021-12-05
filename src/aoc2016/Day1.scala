package aoc2016

import scala.io.Source
import scala.util.Using

object Day1 extends App {
  val input = Using(Source.fromFile("input/2016/1.txt")) {
    _.mkString
  }.get

  val instruction = raw"(R|L)(\d+)".r
  val instructions = input.split(", ").map {
    case instruction(dir, steps) => (if (dir == "R") 1 else -1, steps.toInt)
  }

  def nextPos(pos: (Int, Int, Int), instruction: (Int, Int)) = {
    val (x, y, dir) = pos
    val (dDir, steps) = instruction
    val newDir = (dir + dDir + 4) % 4
    newDir match {
      case 0 => (x, y - steps, newDir)
      case 1 => (x + steps, y, newDir)
      case 2 => (x, y + steps, newDir)
      case 3 => (x - steps, y, newDir)
    }
  }

  val (x1, y1, _) = instructions.foldLeft(0, 0, 0)(nextPos)
  val part1 = Math.abs(x1) + Math.abs(y1)
  println(part1)

  val visited = scala.collection.mutable.Set[(Int, Int)]()
  val (x2, y2, _) = instructions.scanLeft(0, 0, 0)(nextPos)
    .foldLeft(Seq((0, 0, 0))) {
      case (visited, (x2, y2, dir)) =>
        val (x1, y1, _) = visited.last
        val xs = x1 to x2 by (if (x1 < x2) 1 else -1)
        val ys = y1 to y2 by (if (y1 < y2) 1 else -1)
        visited ++ (for (x <- xs; y <- ys) yield (x, y, dir)).tail
    }
    .dropWhile { case (x, y, _) => visited.add(x, y)}
    .head
  val part2 = Math.abs(x2) + Math.abs(y2)
  println(part2)

}
