package aoc2024

import scala.io.Source
import scala.util.Using

object Day2 extends App {

  val input = Using(Source.fromFile("input/2024/2.txt")) {
    _.getLines().toSeq
  }.get

  val reports = input.map(line => {
    line.split(" ").map(_.toInt).toSeq
  })


  def isSafe(report: Seq[Int]): Boolean = {
    val diffs = report.zip(report.tail).map{ case (curr, next) => next - curr}
    val allIncrease = diffs.forall(diff => diff >=1 && diff <=3)
    val allDecrease = diffs.forall(diff => diff >= -3 && diff <= -1)
    allDecrease || allIncrease
  }

  val part1 = reports.count(isSafe)
  println(part1)

  val part2 = reports.count(report => {
    val allReports = report +: report.indices.map(i => report.zipWithIndex.filterNot{case (_, index) => index == i}.map(_._1))
    allReports.exists(isSafe)
  })

  println(part2)

}
