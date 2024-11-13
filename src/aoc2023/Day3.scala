package aoc2023

import scala.io.Source
import scala.util.Using

object Day3 extends App {

  val input = Using(Source.fromFile("input/2023/3.txt")) {
    _.getLines().toSeq
  }.get

  @scala.annotation.tailrec
  def numberPositions(line: String, fromIndex: Int, acc: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    val index = line.indexWhere(_.isDigit, fromIndex)
    if (index == -1)
      acc
    else {
      val length = line.drop(index).takeWhile(_.isDigit).length
      numberPositions(line, index + length, acc :+ (index, index + length))
    }
  }

  val adjacentSymbols = input.zipWithIndex.flatMap {
    case (line, i) =>
      val numberPos = numberPositions(line, 0, Seq.empty)
      numberPos.map {
        case (start, end) =>
          val symbols = for (
            x <- start - 1 to end if line.indices.contains(x);
            y <- i - 1 to i + 1 if input.indices.contains(y)
            if !input(y).charAt(x).isDigit
            if input(y).charAt(x) != '.'
          ) yield (x, y, input(y).charAt(x))
          (i, start, end) -> symbols
      }
  }

  val part1 = adjacentSymbols.filter {
    case (_, symbols) => symbols.nonEmpty
  }.map {
    case ((i, start, end), _) => input(i).substring(start, end).toInt
  }.sum

  println(part1)

  val part2 = adjacentSymbols
    .flatMap(_._2)
    .filter(_._3 == '*')
    .distinct
    .map(symbol => adjacentSymbols.filter(_._2.contains(symbol)).map(_._1))
    .filter(_.size == 2)
    .map(_.map {
        case (i, start, end) => input(i).substring(start, end).toInt
      }.product
    )
    .sum

  println(part2)

}

