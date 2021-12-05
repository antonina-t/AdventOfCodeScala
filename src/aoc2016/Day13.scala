package aoc2016

import scala.Function.tupled

object Day13 extends App {
  val input = 1350

  def isWall(x: Int, y: Int) =
    x < 0 || y < 0 ||
      (x * x + 3 * x + 2 * x * y + y + y * y + input).toBinaryString.count(_ == '1') % 2 == 1

  def neighbours(x: Int, y: Int) =
    Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).filterNot(tupled(isWall)).toSet

  def discover() = LazyList.from(1)
    .scanLeft((Set((1, 1)), Set((1, 1)))) { case ((layer, visited), _) =>
      val nextLayer = layer.flatMap(tupled(neighbours)).diff(visited)
      (nextLayer, visited ++ nextLayer)
    }

  val part1 = discover()
    .takeWhile(!_._1.contains(31, 39))
    .length
  println(part1)

  val part2 = discover()
    .drop(50)
    .head._2
    .size
  println(part2)
}
