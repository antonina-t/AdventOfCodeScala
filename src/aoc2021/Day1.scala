package aoc2021

import scala.io.Source
import scala.util.Using

object Day1 extends App {
  val input = Using(Source.fromFile("input/2021/1.txt")) {
    _.getLines.toSeq
  }.get.map(_.toInt)

  def nbrOfIncreases(input: Seq[Int]) =
    input.zip(input.drop(1)).count(pair => pair._1 < pair._2)

  val part1 = nbrOfIncreases(input)
  println(part1)

  val sum3 = (0 until input.size - 2).map(i => input.slice(i, i + 3).sum)
  val part2 = nbrOfIncreases(sum3)
  println(part2)

}
