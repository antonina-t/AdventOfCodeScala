package aoc2016

import scala.io.Source
import scala.util.Using

object Day9 extends App {
  val input = Using(Source.fromFile("input/2016/9.txt")) {
    _.mkString.trim
  }.get

  val token = raw"([^\(\)]*)\((\d+)x(\d+)\)(.*)".r

  def length1(s: String): Int = s match {
    case token(start, chars, reps, rest) =>
      start.length + chars.toInt * reps.toInt + length1(rest.drop(chars.toInt))
    case _ => s.length
  }

  val part1 = length1(input)
  println(part1)

  def length2(s: String): Long = s match {
    case token(start, chars, reps, rest) =>
      start.length.toLong + length2(rest.take(chars.toInt)) * reps.toInt + length2(rest.drop(chars.toInt))
    case _ => s.length
  }

  val part2 = length2(input)
  println(part2)

}
