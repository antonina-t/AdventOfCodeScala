package aoc2015

import scala.io.Source
import scala.util.Using

object Day9 extends App {
  val input = Using(Source.fromFile("input/2015/9.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = raw"(.*) to (.*) = (\d+)".r

  def key(a: String, b: String) = Seq(a, b).sorted.mkString("-")

  val distances = input.map {
    case pattern(a, b, distance) => key(a, b) -> distance.toInt
  }.toMap

  val cities = input.flatMap {
    case pattern(a, b, _) => Seq(a, b)
  }.distinct

  def getRoutes(cities: Seq[String]): Seq[Seq[String]] = cities match {
    case Seq() => Seq()
    case Seq(a) => Seq(Seq(a))
    case _ => cities.flatMap(city => getRoutes(cities.filterNot(_ == city)).map(city +: _))
  }

  val routeDistances = getRoutes(cities)
    .map(route => route
      .zip(route.tail)
      .map {
        case (a, b) => distances(key(a, b))
      }
      .sum)

  val part1 = routeDistances.min
  println(part1)

  val part2 = routeDistances.max
  println(part2)
}
