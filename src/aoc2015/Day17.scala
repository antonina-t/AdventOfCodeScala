package aoc2015

import scala.io.Source
import scala.util.Using

object Day17 extends App {
  val input = Using(Source.fromFile("input/2015/17.txt")) {
    _.getLines.toSeq
  }.get

  val totalCap = 150
  val containers = input.map(_.toInt).sorted.reverse

  val combinations = containers.foldLeft(Seq(Seq[Int]()))((acc, container) => {
    acc.flatMap(combo =>
      if (combo.sum + container <= totalCap)
        Seq(combo, combo :+ container)
      else
        Seq(combo)
    )
  }).filter(_.sum == totalCap)

  val part1 = combinations.length
  println(part1)

  val part2 = combinations.groupBy(_.length).minBy(_._1)._2.length
  println(part2)
}
