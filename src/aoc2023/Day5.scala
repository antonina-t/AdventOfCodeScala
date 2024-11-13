package aoc2023

import scala.io.Source
import scala.util.Using

object Day5 extends App {

  val input = Using(Source.fromFile("input/2023/5.txt")) {
    _.getLines().mkString("\n")
  }.get

  val sections = input.split("\n\n").map(_.split("\n"))

  val seeds = sections(0)(0).split(": ")(1).split(" ").map(_.toLong)

  val maps = sections.tail.map(_.tail.map(_.split(" ").map(_.toLong)))

  val part1 = seeds.map(seed => maps.foldLeft(seed){
    case (acc, map) =>
      map.find {
        case Array(_, src, length) => acc >= src && acc < src + length
      }.map {
        case Array(dest, src, _) => dest + acc - src
      }.getOrElse(acc)
  }).min

  println(part1)

  def split(interval: (Long, Long), map: Array[Array[Long]]): Seq[(Long, Long)] = {
    val (iStart, iLength) = interval
    map.find {
          // interval inside another
      case Array(dest, src, length) => iStart >= src && iStart + iLength <= src + length
    }.map {
      case Array(dest, src, length) => Seq((dest + iStart - src, iLength))
    }.orElse(map.find{
          // interval intersects on the right side
      case Array(dest, src, length) => iStart >= src && iStart < src + length && iStart + iLength > src + length
    }.map{
      case Array(dest, src, length) => (dest + iStart - src, src + length - iStart) +: split((src + length, iStart + iLength - src - length), map)
    }).orElse(map.find{
          // another inside this interval
      case Array(dest, src, length) => iStart < src && iStart + iLength > src + length
    }.map{
      case Array(dest, src, length) => ((dest, length) +: split((iStart, src - iStart), map)) ++ split((src + length, iStart + iLength - src - length), map)
    }).orElse(map.find{
          // interval intersects on the left side
      case Array(dest, src, length) => iStart < src && iStart + iLength > src
    }.map{
      case Array(dest, src, length) => (dest, iStart + iLength - src) +: split((iStart, src - iStart), map)
    }).getOrElse(Seq(interval))
  }

  val intervals = seeds.sliding(2, 2).map(arr => (arr(0), arr(1))).toSeq

  val part2 = maps.foldLeft(intervals){
    case (acc, map) =>
      acc.flatMap(interval => split(interval, map))
  }.map(_._1).min

  println(part2)
}

