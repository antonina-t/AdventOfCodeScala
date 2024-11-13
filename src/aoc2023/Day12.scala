package aoc2023

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input = Using(Source.fromFile("input/2023/12.txt")) {
    _.getLines().toSeq
  }.get

  val memo = collection.mutable.Map.empty[(String, Seq[Int]), Long]

  def countCombos(pattern: String, groups: Seq[Int]): Long = {
    if (pattern.length < groups.sum + groups.size - 1)
      0
    else if (memo.contains((pattern, groups)))
      memo((pattern, groups))
    else if (groups.isEmpty) {
      val res = if (pattern.contains('#')) 0 else 1
      memo += ((pattern, groups) -> res)
      res
    } else {
      val group = groups.head
      val prefixMatches = !pattern.take(group).contains(".")
      val matchAtFirstChar = if (prefixMatches && !pattern.drop(group).headOption.contains('#')) {
        countCombos(pattern.drop(group + (if (groups.size > 1) 1 else 0)), groups.tail)
      } else 0
      val noMatchAtFirstChar = if (pattern.head != '#') {
        countCombos(pattern.tail, groups)
      } else 0
      val res = matchAtFirstChar + noMatchAtFirstChar
      memo += ((pattern, groups) -> res)
      res
    }
  }

  val part1 = input.map { s =>
    val s"$springs $groups" = s
    val combos = countCombos(springs, groups.split(",").map(_.toInt))
    combos
  }.sum

  println(part1)

  val part2 = input.map { s =>
    val s"$springs $groups" = s
    val springsUnfolded = Iterator.continually(springs).take(5).mkString("?")
    val groupsUnfolded = Iterator.continually(groups).take(5).mkString(",")
    val combos = countCombos(springsUnfolded, groupsUnfolded.split(",").map(_.toInt))
    combos
  }.sum

  println(part2)

}

