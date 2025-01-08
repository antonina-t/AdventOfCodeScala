package aoc2024

import scala.io.Source
import scala.util.Using

object Day18 extends App {

  val input = Using(Source.fromFile("input/2024/18.txt")) {
    _.getLines().toSeq
  }.get

  val allBytes = input.map(_.split(",").map(_.toInt)).map(a => P(a(1), a(0)))


  def path(visited: Set[P], next: Set[P], length: Int, bytes: Set[P]): Int = {
    if (next.isEmpty)
      -1
    else if (next.contains(P(70, 70)))
      length
    else {
      val newNext = next.flatMap(p => {
        Seq(P(p.row, p.col + 1), P(p.row + 1, p.col), P(p.row, p.col - 1), P(p.row - 1, p.col))
          .filterNot(bytes.contains)
          .filterNot(visited.contains)
          .filter(p => p.row >= 0 && p.row <= 70 && p.col >= 0 && p.col <= 70)
      })
      path(visited ++ next, newNext, length + 1, bytes)
    }
  }

  val part1 = path(Set.empty, Set(P(0,0)), 0, allBytes.take(1024).toSet)
  println(part1)

  val i = (1025 to allBytes.length).find(i => {
    val bytes = allBytes.take(i).toSet
    path(Set.empty, Set(P(0,0)), 0, allBytes.take(i).toSet) == -1
  }).get

  val part2 = input(i - 1)
  println(part2)



}
