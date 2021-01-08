package aoc2015

import scala.io.Source
import scala.util.Using

object Day8 extends App {
  val input = Using(Source.fromFile("input/2015/8.txt")) {
    _.getLines.toSeq
  }.get

  val decodedLength = input.map(_
    .replaceAll("^\"|\"$", "") // remove surrounding ""
    .replaceAll("""\\\\""", "/") // replace \\ with /
    .replaceAll("""\\"""", "\"") // replace \" with "
    .replaceAll("\\\\x..", "#") // replace \x** (where ** are any two characters) with #
    .length
  ).sum

  var part1 = input.map(_.length).sum - decodedLength
  println(part1)

  val encodedLength = input.map(s => s.length + s.count(c => c == '\\' || c == '"') + 2).sum

  val part2 = encodedLength - input.map(_.length).sum
  println(part2)
}
