package aoc2016

import scala.io.Source
import scala.util.Using

object Day15 extends App {
  val input = Using(Source.fromFile("input/2016/15.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = raw"Disc .* has (\d+) positions; at time=0, it is at position (\d+)\.".r


}
