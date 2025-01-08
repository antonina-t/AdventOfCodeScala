package aoc2024

import scala.io.Source
import scala.util.Using

object Day21 extends App {

  val input = Using(Source.fromFile("input/2024/21e.txt")) {
    _.getLines().toSeq
  }.get

  /*
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+
  */

  val numPad = Map(
    "7" -> P(0,0),
    "8" -> P(0,1),
    "9" -> P(0,2),
    "4" -> P(1,0),
    "5" -> P(1,1),
    "6" -> P(1,2),
    "1" -> P(2,0),
    "2" -> P(2,1),
    "3" -> P(2,2),
    "0" -> P(3,1),
    "A" -> P(3,2),
  )

  def sequence(src: String, dst: String, pad: Map[String, P]): String = {
    val (P(aRow, aCol), P(bRow, bCol)) = (pad(src), pad(dst))
    val (dCol, dRow) = (bCol - aCol, bRow - aRow)
    val (v, h) = ("v" * dRow + "^" * -dRow, ">" * dCol + "<" * -dCol)
    if ((dCol < 0 || !pad.values.exists(_ == P(bRow, aCol))) && pad.values.exists(_ == P(aRow, bCol)))
      s"$h${v}A"
    else
      s"$v${h}A"
  }

  /*
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+
  */

  val dirPad = Map(
    "^" -> P(0,1),
    "A" -> P(0,2),
    "<" -> P(1,0),
    "v" -> P(1,1),
    ">" -> P(1,2),
  )

  val memo = collection.mutable.Map.empty[(String, String, Int),BigInt]
  def length(from: String, to: String, count: Int): BigInt = {
    val key = (from, to, count)
    if (memo.contains(key)) memo(key)
    else if (count == 0) 1
    else {
      val presses = sequence(from, to, dirPad)
      val result = ("A" + presses).zip(presses).map(pair => length(pair._1.toString, pair._2.toString, count - 1)).sum
      memo(key) = result
      result
    }
  }

  def solve(dirPadRobots: Int) = {
    input.map(code => {
      val num = ("A" + code).zip(code).map(pair => sequence(pair._1.toString, pair._2.toString, numPad)).mkString
      val l = ("A" + num).zip(num).map(pair => length(pair._1.toString, pair._2.toString, dirPadRobots)).sum
      l * code.take(3).toInt
    }).sum
  }

  val part1 = solve(2)
  println(part1)

  val part2 = solve(25)
  println(part2)

}
