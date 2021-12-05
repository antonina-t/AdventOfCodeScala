package aoc2016

import scala.io.Source
import scala.util.Using

object Day3 extends App {
  val input = Using(Source.fromFile("input/2016/3.txt")) {
    _.getLines.toSeq
  }.get

  val triangle = raw"\s*(\d+)\s*(\d+)\s*(\d+)".r

  val triangles = input.map { case triangle(a, b, c) => (a.toInt, b.toInt, c.toInt) }

  def isValid(t: (Int, Int, Int)) = t match {
    case (a, b, c) => a + b > c && a + c > b && b + c > a
  }

  val part1 = triangles.count(isValid)
  println(part1)

  val part2 = triangles
    .grouped(3)
    .flatMap { case Seq((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) => Seq((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)) }
    .count(isValid)
  println(part2)
}
