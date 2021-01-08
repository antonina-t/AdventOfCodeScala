package aoc2015

import scala.io.Source
import scala.util.Using

object Day6 extends App {
  val input = Using(Source.fromFile("input/2015/6.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = raw"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)".r

  def updateGrid[T](getUpdate: String => T => T)(grid: Array[Array[T]], s: String): Array[Array[T]] = s match {
    case pattern(values@_*) =>
      val Seq(x1, y1, x2, y2) = values.tail.map(_.toInt)
      val allPoints = for (x <- x1 to x2; y <- y1 to y2) yield (x, y)
      allPoints.foreach {
        case (x, y) => grid(x)(y) = getUpdate(values.head)(grid(x)(y))
      }
      grid
  }

  val updates1 = Map[String, Boolean => Boolean](
    "turn on" -> (_ => true),
    "turn off" -> (_ => false),
    "toggle" -> (!_)
  )

  val part1 = input
    .foldLeft(Array.ofDim[Boolean](1000, 1000))(updateGrid(updates1))
    .map(_.count(v => v)).sum
  println(part1)

  val updates2 = Map[String, Int => Int](
    "turn on" -> (_ + 1),
    "turn off" -> (v => if (v > 0) v - 1 else 0),
    "toggle" -> (_ + 2)
  )

  val part2 = input.foldLeft(Array.ofDim[Int](1000, 1000))(updateGrid(updates2))
    .map(_.sum).sum
  println(part2)

}
