package aoc2022

import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val input = Using(Source.fromFile("input/2022/10.txt")) {
    _.getLines().toSeq
  }.get

  val xValues = input.foldLeft(Seq(1, 1)) {
    case (values, line) =>
      val x = values.last
      line match {
        case "noop" => values :+ x
        case _ =>
          val delta = line.split(" ")(1).toInt
          (values :+ x) :+ (x + delta)
      }
  }

  val part1 = Seq(20, 60, 100, 140, 180, 220).map(cycle => xValues(cycle) * cycle).sum
  println(part1)

  xValues.indices.tail.foreach { cycle =>
    val col = (cycle - 1) % 40
    if (col == 0) println()
    if ((col - 1 to col + 1).contains(xValues(cycle)))
      print("#")
    else
      print(".")
  }

}

