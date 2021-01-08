package aoc2015

import scala.io.Source
import scala.util.Using

object Day16 extends App {
  val input = Using(Source.fromFile("input/2015/16.txt")) {
    _.getLines.toSeq
  }.get

  def target = Seq(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1
  )

  val pattern = raw"Sue (\d+): (.*)".r
  val compounds = raw"(\w+): (\d+)".r

  val sues = input.map {
    case pattern(n, data) => n -> compounds
      .findAllIn(data)
      .matchData
      .map(_.subgroups match { case Seq(c, v) => c -> v.toInt })
      .toSeq
  }

  val part1 = sues.maxBy { case (_, compounds) => compounds.count(target.contains(_)) }._1
  println(part1)

  val target2: Map[String, Int => Boolean] = Map(
    "children" -> (_ == 3),
    "cats" -> (_ > 7),
    "samoyeds" -> (_ == 2),
    "pomeranians" -> (_ < 3),
    "akitas" -> (_ == 0),
    "vizslas" -> (_ == 0),
    "goldfish" -> (_ < 5),
    "trees" -> (_ > 3),
    "cars" -> (_ == 2),
    "perfumes" -> (_ == 1)
  )

  val part2 = sues.maxBy { case (_, compounds) => compounds.count { case (c, v) => target2(c)(v) } }._1
  println(part2)
}
