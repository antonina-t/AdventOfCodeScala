package aoc2021

import scala.io.Source
import scala.util.Using

object Day12 extends App {
  val input = Using(Source.fromFile("input/2021/12.txt")) {
    _.getLines.toSeq
  }.get.map(_.split("-"))

  val lookup1 = input.groupBy(_ (0)).map { case (k, v) => (k, v.map(_ (1)).toSet) }
  val lookup2 = input.groupBy(_ (1)).map { case (k, v) => (k, v.map(_ (0)).toSet) }
  def lookup(k: String) = lookup1.getOrElse(k, Set.empty) ++ lookup2.getOrElse(k, Set.empty)

  def getPaths(paths: Seq[Seq[String]]): Seq[Seq[String]] = {
    if (paths.forall(p => p.last == "end" || lookup(p.last).diff(p.filter(_ (0).isLower).toSet).isEmpty)) paths
    else {
      val finishedPaths = paths.filter(_.last == "end")
      val newPaths = paths.filterNot(_.last == "end").flatMap { p =>
        lookup(p.last).diff(p.filter(_(0).isLower).toSet).map(n => p :+ n)
      }
      getPaths(finishedPaths ++ newPaths)
    }
  }

  def next(p: Seq[String]): Set[String] = if (p.last == "end") Set.empty
  else {
    if (p.filter(_(0).isLower).groupBy(identity).exists(_._2.size > 1))
      lookup(p.last).diff(p.filter(_(0).isLower).toSet)
    else lookup(p.last).filterNot(_ == "start")
  }

  def getPaths2(paths: Seq[Seq[String]]): Seq[Seq[String]] = {
    if (paths.forall(p => p.last == "end" || next(p).isEmpty)) paths
    else {
      val finishedPaths = paths.filter(_.last == "end")
      val newPaths = paths.filterNot(_.last == "end").flatMap { p =>
        next(p).map(n => p :+ n)
      }
      getPaths2(finishedPaths ++ newPaths)
    }
  }

  val part1 = getPaths(Seq(Seq("start"))).count(_.last == "end")
  println(part1)

  val part2 = getPaths2(Seq(Seq("start"))).count(_.last == "end")
  println(part2)

}
