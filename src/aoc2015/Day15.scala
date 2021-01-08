package aoc2015

import scala.io.Source
import scala.util.Using

object Day15 extends App {
  val input = Using(Source.fromFile("input/2015/15.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = ".*: capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)".r

  val data = input.map(pattern.findAllIn(_).subgroups.init.map(_.toInt))

  val partitions =
    for (a <- 0 to 100; b <- 0 to 100 - a; c <- 0 to 100 - a - b)
      yield Seq(a, b, c, 100 - a - b - c)

  def findBestRecipe(partitions: Seq[Seq[Int]]) = partitions.map(p =>
    data.indices
      .map(i => p
        .zip(data.map(_ (i)))
        .map {
          case (quantity, value) => quantity * value
        }
        .sum)
      .map(sum => if (sum > 0) sum else 0)
      .product)
    .max

  val part1 = findBestRecipe(partitions)
  println(part1)

  val calories = input.map(pattern.findAllIn(_).subgroups.last.toInt)
  val partitions500cal = partitions
    .filter(_
      .zip(calories)
      .map { case (qty, value) => qty * value }
      .sum == 500)

  val part2 = findBestRecipe(partitions500cal)
  println(part2)
}
