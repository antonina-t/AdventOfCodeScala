package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day3 extends App {
  val input = Using(Source.fromFile("input/2021/3e.txt")) {
    _.getLines.toSeq
  }.get

  def mostCommonAt(input: Seq[String], pos: Int) = input.groupBy(_.charAt(pos)).maxBy(_._2.size)._1
  def invert(c: Char) = if (c == '0') '1' else '0'

  val gamma = input.head.indices.map(i => mostCommonAt(input, i)).mkString
  val epsilon = gamma.map(invert).mkString

  val gamma10 = Integer.parseInt(gamma, 2)
  val epsilon10 = Integer.parseInt(epsilon, 2)

  val part1 = gamma10 * epsilon10
  println(part1)

  @tailrec
  def filter(input: Seq[String], pos: Int, mostCommon: Boolean): Seq[String] = {
    if (input.size == 1) input else {
      val mostCommonDigit = mostCommonAt(input, pos)
      val c = if (mostCommon) mostCommonDigit else invert(mostCommonDigit)
      filter(input.filter(_.charAt(pos) == c), pos + 1, mostCommon)
    }
  }

  val a = filter(input, 0, true).head
  val b = filter(input, 0, false).head

  val a10 = Integer.parseInt(a, 2)
  val b10 = Integer.parseInt(b, 2)

  val part2 = a10 * b10
  println(part2)

}
