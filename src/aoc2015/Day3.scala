package aoc2015

import scala.io.Source
import scala.util.Using

object Day3 extends App {
  val input = Using(Source.fromFile("input/2015/3.txt")) {
    _.mkString
  }.get

  def move(from: (Int, Int), dir: Char): (Int, Int) = {
    val (x, y) = from
    dir match {
      case '^' => (x, y - 1)
      case '>' => (x + 1, y)
      case 'v' => (x, y + 1)
      case '<' => (x - 1, y)
    }
  }

  def visited(seq: Seq[Char]) = seq.scanLeft((0, 0))(move).toSet

  val part1 = visited(input).size
  println(part1)

  def everySecond(seq: Seq[Char], drop: Int) = seq.drop(drop).zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

  val santa = visited(everySecond(input, 0))
  val roboSanta = visited(everySecond(input, 1))
  val part2 = (santa ++ roboSanta).size
  println(part2)
}
