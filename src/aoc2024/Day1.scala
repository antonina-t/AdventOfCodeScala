package aoc2024

import scala.io.Source
import scala.util.Using

object Day1 extends App {

  val input = Using(Source.fromFile("input/2024/1.txt")) {
    _.getLines().toSeq
  }.get

  val lines = input.map(s => s.split(" ").filter(_.nonEmpty).map(_.toInt))

  val list1 = lines.map(_(0)).sorted
  val list2 = lines.map(_(1)).sorted

  val part1 = (list1 zip list2).map {
    case (n1, n2) => Math.abs(n1 - n2)
  }.sum

  println(part1)
  
  val part2 = list1.map(n => list2.count(_ == n) * n).sum
  println(part2)

}
