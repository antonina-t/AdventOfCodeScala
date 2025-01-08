package aoc2024

import scala.io.Source
import scala.util.Using

object Day8 extends App {

  val input = Using(Source.fromFile("input/2024/8.txt")) {
    _.getLines().toSeq
  }.get

  val antennas = input.flatMap(_.filter(_ != '.')).toSet

  def pairs(list: Seq[(Int, Int)]): Seq[((Int, Int), (Int, Int))] = {
    if (list.size < 2) Seq.empty
    else
      list.tail.map(el => (list.head, el)) ++ pairs(list.tail)
  }

  def antinodes(a1: (Int, Int), a2: (Int, Int), part2: Boolean) = {
    val byRow = Seq(a1, a2).sortBy(_._1)
    val rowDiff = byRow.last._1 - byRow.head._1
    val colDiff = byRow.last._2 - byRow.head._2

    val range = if (part2) (0 to 1000) else (1 to 1)

    val firstL = range.map { mul =>
      (byRow.head._1 - mul * rowDiff, byRow.head._2 - mul * colDiff)
    }.takeWhile(a => input.indices.contains(a._1) && input.head.indices.contains(a._2))
    val secondL = range.map { mul =>
      (byRow.last._1 + mul * rowDiff, byRow.last._2 + mul * colDiff)
    }.takeWhile(a => input.indices.contains(a._1) && input.head.indices.contains(a._2))

    firstL ++ secondL
  }

  def antinodeCount(part2: Boolean) =
    antennas.foldLeft(Set.empty[(Int, Int)])((acc, antenna) => {
    val allAntennas = input.indices.flatMap(row => input(row).indices.filter(col => input(row)(col) == antenna).map(col => (row, col)))
    val antennaPairs = pairs(allAntennas)
    acc ++ antennaPairs.flatMap {
      case (a1, a2) => antinodes(a1, a2, part2)
    }.toSet.filter(a => input.indices.contains(a._1) && input.head.indices.contains(a._2))
  }).size

  val part1 = antinodeCount(false)
  println(part1)

  val part2 = antinodeCount(true)
  println(part2)

}
