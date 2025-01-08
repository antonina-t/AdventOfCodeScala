package aoc2024

import scala.io.Source
import scala.util.Using

object Day4 extends App {

  val input = Using(Source.fromFile("input/2024/4.txt")) {
    _.getLines().toSeq
  }.get

  val pattern = "XMAS".r

  def count(lines: Seq[String]) = lines.map(line => pattern.findAllIn(line).size + pattern.findAllIn(line.reverse).size).sum

  val rows = input
  val cols = input.indices.map(i => input.map(_(i)).mkString)
  val diags1 = input.indices.map(row => (0 to row).zip(row to 0 by -1).map(rc => input(rc._1)(rc._2)).mkString)
  val diags2 = input.indices.map(row => (input.indices.reverse).zip(row until input.head.length).map(rc => input(rc._1)(rc._2)).mkString).tail
  val diags3 = input.indices.map(row => (row until input.length).zip(input.indices).map(rc => input(rc._1)(rc._2)).mkString)
  val diags4 = input.indices.map(row => (input.indices).zip(row until input.head.length).map(rc => input(rc._1)(rc._2)).mkString).tail


  val part1 = count(input) + count(cols) + count(diags1) + count(diags2) + count(diags3) + count(diags4)
  println(part1)

  val vals = Seq("MSAMS", "MMASS", "SSAMM", "SMASM")

  val part2 = (for (row <- input.indices; col <- input.head.indices) yield (row, col)).count {
    case (row, col) =>
      input.indices.contains(row + 2) && input.head.indices.contains(col + 2) &&
        vals.contains(Seq(input(row)(col), input(row)(col + 2), input(row + 1)(col + 1), input(row + 2)(col), input(row + 2)(col + 2)).mkString)
  }

  println(part2)
}
