package aoc2015

import scala.io.Source
import scala.util.Using

object Day5 extends App {
  val input = Using(Source.fromFile("input/2015/5.txt")) {
    _.getLines.toSeq
  }.get

  val conditions1 = Seq[String => Boolean](
    s => s.count("aeiou".contains(_)) >= 3,
    s => s.zip(s.tail).exists { case (a, b) => a == b },
    s => Seq("ab", "cd", "pq", "xy").forall(!s.contains(_))
  )

  val part1 = input.count(s => conditions1.forall(c => c(s)))
  println(part1)

  val conditions2 = Seq[String => Boolean](
    s => s.indices.map(drop => s.drop(drop)).exists(s => s.drop(2).contains(s.take(2))),
    s => s.zip(s.drop(2)).exists { case (a, b) => a == b }
  )

  val part2 = input.count(s => conditions2.forall(c => c(s)))
  println(part2)
}
