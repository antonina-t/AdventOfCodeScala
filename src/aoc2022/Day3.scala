package aoc2022

import scala.io.Source
import scala.util.Using

object Day3 extends App {

  val input = Using(Source.fromFile("input/2022/3.txt")) {
    _.getLines().toSeq
  }.get

  def toPrio(c: Char) = if (c.isLower) c - 'a' + 1 else c - 'A' + 27

  val part1 = input.map(line => {
    val left = line.take(line.length / 2)
    val right = line.drop(line.length / 2)
    left.intersect(right).head
  }).map(toPrio).sum
  println(part1)

  val part2 = input.sliding(3, 3).map {
    case Seq(first, second, third) => first.intersect(second).intersect(third).head
  }.map(toPrio).sum
  println(part2)

}

