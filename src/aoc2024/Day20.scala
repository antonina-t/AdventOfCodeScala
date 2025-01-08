package aoc2024

import aoc2024.Day20.path

import scala.io.Source
import scala.util.Using

object Day20 extends App {

  val input = Using(Source.fromFile("input/2024/20.txt")) {
    _.getLines().toSeq
  }.get


  def validate(p: P) = {
    input.indices.contains(p.row) && input.head.indices.contains(p.col) && "SE.".contains(p.c(input))
  }

  def go(path: Seq[P]): Seq[P] = {
    val p = path.last
    if (p.c(input) == 'E')
      path
    else {
        val next = Seq(P(p.row, p.col + 1), P(p.row + 1, p.col), P(p.row, p.col - 1), P(p.row - 1, p.col))
          .filter(p => validate(p) && !path.init.lastOption.contains(p)).head
        go(path :+ next)
    }
  }

  val startRow = input.indexWhere(_.contains("S"))
  val start = P(startRow, input(startRow).indexOf("S"))
  val path = go(Seq(start))

  val allP = for (row <- input.indices; col <- input.head.indices) yield P(row, col)
  val cheats = allP.filter(validate).flatMap(p => {
    val next = Seq(P(p.row, p.col + 2), P(p.row + 2, p.col), P(p.row, p.col - 2), P(p.row - 2, p.col)).filter(validate)
    next.map(n => (p, n))
  })

  val part1 = cheats.filter(c => path.contains(c._1) && path.contains(c._2) && path.indexOf(c._2) > path.indexOf(c._1))
    .count(c => path.indexOf(c._2) - path.indexOf(c._1) - 2 >= 100)
  println(part1)

  val cheats2 = allP.filter(validate).flatMap(p => {
    val deltas = (2 to 20).flatMap(max => for (r <- 0 to max; c <- 0 to max if r + c == max) yield (r, c))
    val next = deltas.flatMap(d => {
      Seq(P(p.row + d._1, p.col + d._2), P(p.row - d._1, p.col + d._2), P(p.row + d._1, p.col - d._2), P(p.row - d._1, p.col - d._2))
        .filter(validate)
    })
    next.map(n => (p, n))
  }).distinct

  val index = path.indices.map(i => path(i) -> i).toMap

  val part2 = cheats2
    .filter(c => index.contains(c._1) && index.contains(c._2) && index(c._2) > index(c._1))
    .map(c => (index(c._2) - index(c._1) - (Math.abs(c._2.row - c._1.row) + Math.abs(c._2.col - c._1.col)), c))
    .filter(_._1 >= 100)
    .groupBy(_._1)
    .map(t => (t._1, t._2.map(_._2).size))
    .toSeq
    .sortBy(_._1)
    .map(_._2)
    .sum
  println(part2)


}
