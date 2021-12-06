package aoc2021

import scala.io.Source
import scala.util.Using

object Day1 extends App {
  val input = Using(Source.fromFile("input/2021/1.txt")) {
    _.getLines.toSeq
  }.get.map(_.toInt)

  def numberOfIncreases(input: Seq[Int]) = input.sliding(2).count { case Seq(a, b) => a < b }

  val part1 = numberOfIncreases(input)
  println(part1)

  val part2 = numberOfIncreases(input.sliding(3).map(_.sum).toSeq)
  println(part2)

}
