package aoc2021

import scala.io.Source
import scala.util.Using

object Day9 extends App {
  val input = Using(Source.fromFile("input/2021/9.txt")) {
    _.getLines.toSeq
  }.get.map(_.map(_ - '0').toArray).toArray

  val indices = (for (y <- input.indices; x <- input(0).indices) yield (y, x)).toSet


  val part1 = input.indices.map(y => input(y).indices.filter(x =>

    (x == 0 || input(y)(x - 1) > input(y)(x)) &&
      (y == 0 || input(y - 1)(x) > input(y)(x)) &&
      (x == input(0).length - 1 || input(y)(x + 1) > input(y)(x)) &&
      (y == input.length - 1 || input(y + 1)(x) > input(y)(x))

  ).map(input(y)(_)).map(_ + 1).sum).sum
  println(part1)



  def getBasin(queue: Set[(Int, Int)], visited: Set[(Int, Int)], size: Int): Int = {
    val newQueue = queue.flatMap { case (y, x) =>
      Set((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1))
        .intersect(indices)
        .diff(visited)
        .filter { case (y1, x1) => input(y1)(x1) > input(y)(x) && input(y1)(x1) != 9 }
    }
    if (newQueue.isEmpty) size else getBasin(newQueue, visited ++ newQueue, size + newQueue.size)
  }

  val bottoms = input.indices.flatMap(y => input(y).indices.filter(x =>
    (x == 0 || input(y)(x - 1) > input(y)(x)) &&
      (y == 0 || input(y - 1)(x) > input(y)(x)) &&
      (x == input(0).length - 1 || input(y)(x + 1) > input(y)(x)) &&
      (y == input.length - 1 || input(y + 1)(x) > input(y)(x))

  ).map((y, _)))


  val part2 = bottoms.map {
    case (y, x) => getBasin(Set((y, x)), Set((y, x)), 1)
  }.sorted.reverse.take(3).product
  println(part2)

}
