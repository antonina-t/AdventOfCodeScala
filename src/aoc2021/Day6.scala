package aoc2021

import scala.io.Source
import scala.util.Using

object Day6 extends App {
  val input = Using(Source.fromFile("input/2021/6.txt")) {
    _.getLines.toSeq
  }.get.head.split(",").map(_.toInt).toSeq

  def simulate(days: Int) = {
    val fish = (0 to 8).map(i => input.count(_ == i).toLong).toArray
    (1 to days).foldLeft(fish)((fish, _) => {
      val newFish = fish.tail :+ fish.head
      newFish(6) = newFish(6) + fish.head
      newFish
    }).sum
  }

  val part1 = simulate(80)
  println(part1)

  val part2 = simulate(256)
  println(part2)

}
