package aoc2021

import scala.io.Source
import scala.util.Using

object Day17 extends App {
  val input = Using(Source.fromFile("input/2021/17.txt")) {
    _.getLines.toSeq
  }.get.head

  val pattern = raw"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)".r

  val (minX, maxX, minY, maxY) = input match {
    case pattern(x1, x2, y1, y2) => (x1.toInt, x2.toInt, y1.toInt, y2.toInt)
  }

  def getPoints(vel: (Int, Int)) =
    LazyList.from(0).scanLeft((0, 0), vel)((v, _) => {
      val ((x, y), (vX, vY)) = v
      ((x + vX, y + vY), (if (vX > 0) vX - 1 else vX, vY - 1))
    }).map(_._1)

  def test(vel: (Int, Int)) = {
    val p = getPoints(vel).dropWhile {
      case (x, y) => (x < maxX && y > maxY) || (x < minX && y > minY)
    }.head
    val (x, y) = p
    x >= minX && x <= maxX && y >= minY && y <= maxY
  }

  val bestVel = (for (x <- 1 to maxX; y <- 1 to Math.abs(minY)) yield (x, y)).filter(test).maxBy(_._2)
  val part1 = getPoints(bestVel).takeWhile(_._2 >= 0).map(_._2).max
  println(part1)

  val part2 = (for (x <- 1 to maxX; y <- minY to Math.abs(minY)) yield (x, y)).count(test)
  println(part2)

}
