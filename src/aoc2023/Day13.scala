package aoc2023

import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("input/2023/13.txt")) {
    _.mkString.split("\n\n").map(_.split("\n").toSeq).toSeq
  }.get

  def horizontalLine(pattern: Seq[String]): Int = {
    pattern.indices.tail.find(i => {
      ((i - 1 to 0 by -1) zip (i until pattern.length)).forall {
        case (row1, row2) => pattern(row1) == pattern(row2)
      }
    }).getOrElse(0)
  }

  def verticalLine(pattern: Seq[String]): Int = {
    pattern.head.indices.tail.find(i => {
      ((i - 1 to 0 by -1) zip (i until pattern.head.length)).forall {
        case (col1, col2) => pattern.forall(row => row.charAt(col1) == row.charAt(col2))
      }
    }).getOrElse(0)
  }

  val part1 = input.map(horizontalLine).sum * 100 + input.map(verticalLine).sum
  println(part1)

  def horizontalLineWithSmudge(pattern: Seq[String]): Int = {
    pattern.indices.tail.find(i => {
      val rowsWithSmudge = ((i - 1 to 0 by -1) zip (i until pattern.length)).filterNot {
        case (row1, row2) => pattern(row1) == pattern(row2)
      }
      if (rowsWithSmudge.size == 1) {
        val (row1, row2) = rowsWithSmudge.head
        (pattern(row1) zip pattern(row2)).count { case (c1, c2) => c1 != c2 } == 1
      } else false
    }).getOrElse(0)
  }

  def verticalLineWithSmudge(pattern: Seq[String]): Int = {
    pattern.head.indices.tail.find(i => {
      val colsWithSmudge = ((i - 1 to 0 by -1) zip (i until pattern.head.length)).filterNot {
        case (col1, col2) => pattern.forall(row => row.charAt(col1) == row.charAt(col2))
      }
      if (colsWithSmudge.size == 1) {
        val (col1, col2) = colsWithSmudge.head
        pattern.count(row => row.charAt(col1) != row.charAt(col2)) == 1
      } else false
    }).getOrElse(0)
  }

  val part2 = input.map(horizontalLineWithSmudge).sum * 100 + input.map(verticalLineWithSmudge).sum
  println(part2)

}

