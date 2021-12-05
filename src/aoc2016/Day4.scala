package aoc2016

import scala.io.Source
import scala.util.Using

object Day4 extends App {
  val input = Using(Source.fromFile("input/2016/4.txt")) {
    _.getLines.toSeq
  }.get

  val room = raw"(.*)-(\d+)\[(.*)\]".r

  val rooms = input.map { case room(name, number, checksum) =>
    (name, number.toInt, checksum)
  }

  def checksum(name: String) = name
    .replaceAll("-", "")
    .groupBy(c => c)
    .values
    .toSeq
    .sortWith((s1, s2) => if (s1.length == s2.length) s1(0) < s2(0) else s1.length > s2.length)
    .map(_(0))
    .take(5)
    .mkString

  val validRooms = rooms.filter { case (name, _, test) => checksum(name) == test }

  val part1 = validRooms.map(_._2).sum
  println(part1)

  val part2 = validRooms.map { case (name, number, _) =>
    name.map(c => if (c == '-') ' ' else ((c - 'a' + number) % 26 + 'a').toChar) -> number
  }
    .filter(_._1 == "northpole object storage")
    .head._2
  println(part2)

}
