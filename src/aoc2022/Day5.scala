package aoc2022

import scala.io.Source
import scala.util.Using

object Day5 extends App {

  val input = Using(Source.fromFile("input/2022/5.txt")) {
    _.getLines().toSeq
  }.get

  val cratePattern = raw"(\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   ) (\[.\]|   )".r

  val initMap = input.take(8).foldLeft(Map.empty[Int, String])((acc, line) => {
    cratePattern.findAllIn(line).subgroups.zip(1 to 9).map {
      case (elem, index) =>
        if (elem.isBlank) index -> acc.getOrElse(index, "")
        else index -> acc.getOrElse(index, "").concat(elem.substring(1, 2))
    }.toMap
  })

  val pattern = raw"move (\d+) from (\d+) to (\d+)".r

  def getResult(reverse: Boolean) = {
    val result = input.drop(10).foldLeft(initMap)((acc, line) => {
      pattern.findAllIn(line).subgroups.map(_.toInt) match {
        case Seq(qty, from, to) =>
          val takenCrates = if (reverse) acc(from).take(qty).reverse else acc(from).take(qty)
          acc ++ Map(to -> (takenCrates ++ acc(to)), from -> acc(from).drop(qty))
      }
    })
    (1 to 9).map(result).map(_.head).mkString
  }
  val part1 = getResult(true)
  println(part1)

  val part2 = getResult(false)
  println(part2)
}

