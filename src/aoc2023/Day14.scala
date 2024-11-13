package aoc2023

import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val input = Using(Source.fromFile("input/2023/14.txt")) {
    _.getLines().toSeq
  }.get

  def tilt(left: Boolean)(s: String) = {
    s.tail.foldLeft(Seq(s.head.toString)) { (chunks, c) => {
      c match {
        case '#' => chunks :+ "#"
        case _ => if (chunks.last == "#") chunks :+ c.toString else chunks.init :+ (chunks.last :+ c)
      }
    }
    }.map(s => if (left) s.sorted.reverse else s.sorted).mkString
  }

  def getWeight(s: String) = s.indices.map(i => {
    (s.length - i) * (if (s.charAt(i) == 'O') 1 else 0)
  }).sum

  val part1 = input.indices.map(i => input.map(_.charAt(i)).mkString).map(tilt(left = true)).map(getWeight).sum
  println(part1)

  def transpose(grid: Seq[String]) = grid.indices.map(i => grid.map(_.charAt(i)).mkString)

  def spin(grid: Seq[String], cycles: Int, states: Seq[Seq[String]]): Seq[String] = {
    if (cycles == 0) grid else {
      if (states.contains(grid)) {
        val cycleStart = states.indexOf(grid)
        val cycleLength = states.size - cycleStart
        val spinsLeft = cycles % cycleLength
        states(cycleStart + spinsLeft)
      } else {
        val north = transpose(grid).map(tilt(left = true))
        val west = transpose(north).map(tilt(left = true))
        val south = transpose(west).map(tilt(left = false))
        val east = transpose(south).map(tilt(left = false))
        spin(east, cycles - 1, states :+ grid)
      }
    }
  }

  val spinned = spin(input, 1000000000, Seq.empty)
  val part2 = transpose(spinned).map(getWeight).sum

  println(part2)

}

