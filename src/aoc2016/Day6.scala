package aoc2016

import scala.io.Source
import scala.util.Using

object Day6 extends App {
  val input = Using(Source.fromFile("input/2016/6.txt")) {
    _.getLines.toSeq
  }.get

  def decrypt(chooseChar: Seq[Char] => Char) = input
    .foldLeft(Map[Int, Seq[Char]]())((acc, s) => s.zipWithIndex.foldLeft(acc) {
      case (acc, (c, i)) => acc + (i -> (acc.getOrElse(i, Seq()) :+ c))
    })
    .toSeq
    .sortBy(_._1)
    .map { case (_, chars) => chooseChar(chars) }
    .mkString

  val part1 = decrypt(_.groupBy(c => c).maxBy(_._2.length)._1)
  println(part1)

  val part2 = decrypt(_.groupBy(c => c).minBy(_._2.length)._1)
  println(part2)
}
