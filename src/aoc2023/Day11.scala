package aoc2023

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val input = Using(Source.fromFile("input/2023/11.txt")) {
    _.getLines().toSeq
  }.get

  val galaxies = for (
    row <- input.indices if input(row).contains("#");
    col <- input(row).indices if input(row)(col) == '#'
  ) yield (row, col)


  val emptyRows = input.indices.filterNot(row => galaxies.exists(_._1 == row))
  val emptyCols = input.head.indices.filterNot(col => galaxies.exists(_._2 == col))

  val pairs = galaxies.tails.filter(_.size >= 2).flatMap(seq => List.fill(seq.tail.size)(seq.head) zip seq.tail).toSeq

  def distances(mult: Int) = {
    pairs.map {
      case (thisGal, otherGal) =>
        val (thisRow, thisCol) = thisGal
        val (otherRow, otherCol) = otherGal
        val distance = 0L + Math.abs(otherRow - thisRow) + Math.abs(otherCol - thisCol)
        val extraRows = emptyRows.count(row => row > Math.min(thisRow, otherRow) && row < Math.max(thisRow, otherRow))
        val extraCols = emptyCols.count(col => col > Math.min(thisCol, otherCol) && col < Math.max(thisCol, otherCol))
        //println(distance + extraRows * mult + extraCols * mult)
        distance + extraRows * mult + extraCols * mult
    }.sum
  }

  val part1 = distances(1)
  println(part1)

  val part2 = distances(999999)
  println(part2)

}

