package aoc2024

import scala.io.Source
import scala.util.Using

object Day19 extends App {

  val input = Using(Source.fromFile("input/2024/19.txt")) {
    _.getLines().toSeq
  }.get

  val towels = input.head.split(", ")

  val designs = input.drop(2)


  val memo = collection.mutable.Map.empty[String, Long]
  def combinationsCount(d: String): Long = {
    //println(d)
    if (memo.contains(d)) memo(d) else {
      if (d.isEmpty)
        1
      else {
        val ts = towels.filter(t => d.startsWith(t))
        if (ts.isEmpty) {
          memo(d) = 0
          0
        } else {
          val result = ts.map(t => combinationsCount(d.drop(t.length))).sum
          memo(d) = result
          result
        }
      }
    }

  }

  val part1 = designs.map(d => {
    if (combinationsCount(d) > 0) 1 else 0
  }).sum
  println(part1)

  val part2 = designs.map(combinationsCount).sum
  println(part1)

}
