package aoc2023

import scala.io.Source
import scala.util.Using

object Day4 extends App {

  val input = Using(Source.fromFile("input/2023/4.txt")) {
    _.getLines().toSeq
  }.get

  val part1 = input.map(line => {
    line.split(": ")(1).split(" \\| ").map(_.split(" ").filter(_.nonEmpty).map(_.toInt)) match {
      case Array(winning, having) => 
        val common = winning.toSet.intersect(having.toSet).size
        if (common == 0) 0 else Math.pow(2, common - 1)
    }
  }).sum
  println(part1)

  val copies = Array.fill(input.size)(1)

  input.zipWithIndex.map{ case (line, i) => {
    line.split(": ")(1).split(" \\| ").map(_.split(" ").filter(_.nonEmpty).map(_.toInt)) match {
      case Array(winning, having) =>
        val common = winning.toSet.intersect(having.toSet).size
        for (j <- 0 until common) copies(i + j + 1) = copies(i + j + 1) + copies(i)
    }
  }}

  val part2 = copies.sum
  println(part2)

}

