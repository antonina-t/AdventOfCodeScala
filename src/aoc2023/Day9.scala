package aoc2023

import scala.io.Source
import scala.util.Using

object Day9 extends App {

  val input = Using(Source.fromFile("input/2023/9.txt")) {
    _.getLines().toSeq
  }.get

  val report = input.map(_.split(" ").map(_.toInt).toSeq)

  def getDeltas(acc: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    if (acc.last.forall(_ == 0))
      acc
    else getDeltas(acc :+ acc.last.sliding(2, 1).map { case Seq(first, second) => second - first}.toSeq)
  }

  val part1 = report.map(history => {
    val deltas = getDeltas(Seq(history))
    deltas.reverse.foldLeft(0L) {
      case (acc, delta) => acc + delta.last
    }
  }).sum

  println(part1)

  val part2 = report.map(history => {
    val deltas = getDeltas(Seq(history))
    deltas.reverse.foldLeft(0L) {
      case (acc, delta) => delta.head - acc
    }
  }).sum

  println(part2)
}

