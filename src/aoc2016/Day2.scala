package aoc2016

import scala.io.Source
import scala.util.Using

object Day2 extends App {
  val input = Using(Source.fromFile("input/2016/2.txt")) {
    _.getLines.toSeq
  }.get

  val keypad1 = Array(
    "-----",
    "-123-",
    "-456-",
    "-789-",
    "-----"
  )

  def getKey(keypad: Array[String], startRow: Int, startCol: Int)(chars: String) = {
    val (row, col) = chars.foldLeft(startRow, startCol) {
      case ((row, col), c) =>
        c match {
          case 'U' => if (keypad(row - 1)(col) == '-') (row, col) else (row - 1, col)
          case 'R' => if (keypad(row)(col + 1) == '-') (row, col) else (row, col + 1)
          case 'D' => if (keypad(row + 1)(col) == '-') (row, col) else (row + 1, col)
          case 'L' => if (keypad(row)(col - 1) == '-') (row, col) else (row, col - 1)
        }
    }
    keypad(row)(col)
  }

  val part1 = input.map(getKey(keypad1, 2, 2))
  println(part1.mkString)

  val keypad2 = Array(
    "-------",
    "---1---",
    "--234--",
    "-56789-",
    "--ABC--",
    "---D---",
    "-------"
  )

  val part2 = input.map(getKey(keypad2, 3, 1))
  println(part2.mkString)

}
