package aoc2023

import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val input = Using(Source.fromFile("input/2023/16.txt")) {
    _.getLines().toSeq
  }.get

  def travel(row: Int, col: Int, dir: Char, visited: Map[(Int, Int), Set[Char]]): Map[(Int, Int), Set[Char]] = {
    if (!input.indices.contains(row) || !input.head.indices.contains(col) || visited.getOrElse((row, col), Set.empty).contains(dir))
      visited
    else {
      val newVisited = visited + ((row, col) -> (visited.getOrElse((row, col), Set.empty) + dir))
      (input(row)(col), dir) match {
        case ('.', '>') | ('-', '>') => travel(row, col + 1, dir, newVisited)
        case ('.', '<') | ('-', '<') => travel(row, col - 1, dir, newVisited)
        case ('.', '^') | ('|', '^') => travel(row - 1, col, dir, newVisited)
        case ('.', 'v') | ('|', 'v') => travel(row + 1, col, dir, newVisited)
        case ('/', '>') => travel(row - 1, col, '^', newVisited)
        case ('/', '<') => travel(row + 1, col, 'v', newVisited)
        case ('/', '^') => travel(row, col + 1, '>', newVisited)
        case ('/', 'v') => travel(row, col - 1, '<', newVisited)
        case ('\\', '>') => travel(row + 1, col, 'v', newVisited)
        case ('\\', '<') => travel(row - 1, col, '^', newVisited)
        case ('\\', '^') => travel(row, col - 1, '<', newVisited)
        case ('\\', 'v') => travel(row, col + 1, '>', newVisited)
        case ('|', '>') | ('|', '<') =>
          val travelUp = travel(row - 1, col, '^', newVisited)
          travel(row + 1, col, 'v', travelUp)
        case ('-', '^') | ('-', 'v') =>
          val travelLeft = travel(row, col - 1, '<', newVisited)
          travel(row, col + 1, '>', travelLeft)
      }
    }
  }

  val traveledGrid = travel(0, 0, '>', Map.empty)
  /*
  for (row <- input.indices; col <- input.head.indices) {
    val dirs = traveledGrid.getOrElse((row, col), Set('.'))
    print(if (dirs.size > 1) dirs.size else dirs.head)
    if (col == input.head.length - 1) println()
  }
  */

  val part1 = traveledGrid.size
  println(part1)

  val sources = input.indices.flatMap(row => Seq((row, 0, '>'), (row, input.size - 1, '<'))) ++
    input.head.indices.flatMap(col => Seq((0, col, 'v'), (input.head.length - 1, col, '^')))

  val part2 = sources.map{ case (row, col, dir) => travel(row, col, dir, Map.empty).size }.max

  println(part2)

}

