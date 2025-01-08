package aoc2024

import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val input = Using(Source.fromFile("input/2024/10.txt")) {
    _.getLines().toSeq
  }.get

  def search(pos: Set[(Int, Int)]): Int = {
    if (pos.isEmpty)
      0
    else {
      val (row, col) = pos.head
      val height = input(row)(col)
      if (height == '9')
        pos.size
      else {
        val next = pos.flatMap(p => {
          Set((p._1 + 1, p._2), (p._1, p._2 + 1), (p._1 - 1, p._2), (p._1, p._2 - 1))
            .filter(v => input.indices.contains(v._1) && input.head.indices.contains(v._2))
            .filter(v => input(v._1)(v._2) == height + 1)
        })
        search(next)
      }
    }
  }

  val start = input.indices.flatMap(row => input(row).indices.filter(col => input(row)(col) == '0').map((row, _)))
  val part1 = start.map(p => search(Set(p))).sum

  println(part1)


  def search2(trails: Seq[Seq[(Int, Int)]]): Int = {
    if (trails.isEmpty)
      0
    else {
      val (row, col) = trails.head.last
      val height = input(row)(col)
      if (height == '9')
        trails.size
      else {
        val next = trails.flatMap(trail => {
          val p = trail.last
          val next = Seq((p._1 + 1, p._2), (p._1, p._2 + 1), (p._1 - 1, p._2), (p._1, p._2 - 1))
            .filter(v => input.indices.contains(v._1) && input.head.indices.contains(v._2))
            .filter(v => input(v._1)(v._2) == height + 1)
          next.map(n => trail :+ n)
        })
        search2(next)
      }
    }
  }

  val part2 = start.map(p => search2(Seq(Seq(p)))).sum

  println(part2)
}
