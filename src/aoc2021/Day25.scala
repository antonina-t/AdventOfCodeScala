package aoc2021

import scala.io.Source
import scala.util.Using

object Day25 extends App {
  val input = Using(Source.fromFile("input/2021/25.txt")) {
    _.getLines.toSeq
  }.get

  @scala.annotation.tailrec
  def solve(state: Seq[String], steps: Int): Int = {
    val sol1 = state.map(_.toCharArray).toArray
    for (y <- state.indices; x <- 0 until state.head.length) {
      state(y)(x) match {
        case '>' => {
          val nextX = (x + 1) % state.head.length
          if (state(y)(nextX) == '.') {
            sol1(y)(nextX) = '>'
            sol1(y)(x) = '.'
          }
        }
        case _ => ()
      }
    }

    val sol2 = sol1.map(_.map(identity))
    for (y <- state.indices; x <- 0 until state.head.length) {
      state(y)(x) match {
        case 'v' =>
          val nextY = (y + 1) % state.length
          if (sol1(nextY)(x) == '.') {
            sol2(nextY)(x) = 'v'
            sol2(y)(x) = '.'
          }
        case _ => ()
      }
    }
    if (sol2.map(_.mkString).mkString == state.mkString) steps + 1 else solve(sol2.map(_.mkString).toSeq, steps + 1)
  }

  val part1 = solve(input, 0)
  println(part1)

}
