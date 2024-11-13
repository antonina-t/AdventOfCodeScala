package aoc2022

import scala.io.Source
import scala.util.Using

object Day22 extends App {

  val input = Using(Source.fromFile("input/2022/22.txt")) {
    _.getLines().toSeq
  }.get

  val gridUnpadded = input.take(input.length - 2)
  val maxLineLength = gridUnpadded.maxBy(_.length).length
  val grid = gridUnpadded.map(line => if (line.length < maxLineLength) line ++ Seq.fill(maxLineLength - line.length)(' ').mkString else line)

  val seq = input.last

  case class P(row: Int, col: Int, dir: Int)

  def findOpposite(p: P): P = {
    val next = p.dir match {
      case 0 => p.copy(col = p.col + 1)
      case 1 => p.copy(row = p.row + 1)
      case 2 => p.copy(col = p.col - 1)
      case 3 => p.copy(row = p.row - 1)
    }
    if (!grid.indices.contains(next.row) || !grid(0).indices.contains(next.col) || grid(next.row)(next.col) == ' ')
      p
    else
      findOpposite(next)
  }


  val path = scala.collection.mutable.Set.empty[P]

  def forward(p: P, steps: Int, isCube: Boolean): P = {
    path.add(p)
    if (steps == 0)
      p
    else {
      val next = p.dir match {
        case 0 => p.copy(col = p.col + 1)
        case 1 => p.copy(row = p.row + 1)
        case 2 => p.copy(col = p.col - 1)
        case 3 => p.copy(row = p.row - 1)
      }

      val isOff = !grid.indices.contains(next.row) || !grid(0).indices.contains(next.col) || grid(next.row)(next.col) == ' '
      if (isOff) {
        val opposite = if (isCube) findCubeOpposite(p) else findOpposite(p.copy(dir = (p.dir + 2) % 4)).copy(dir = (p.dir + 2) % 4)
        if (grid(opposite.row)(opposite.col) == '#')
          p
        else
          forward(opposite, steps - 1, isCube)
      } else if (grid(next.row)(next.col) == '#') {
        p
      } else {
        forward(next, steps - 1, isCube)
      }
    }
  }


  def move(p: P, seq: String, isCube: Boolean): P = {
    path.add(p)
    if (seq.isEmpty)
      p
    else if (seq.head == 'L')
      move(p.copy(dir = (p.dir + 3) % 4), seq.tail, isCube)
    else if (seq.head == 'R')
      move(p.copy(dir = (p.dir + 1) % 4), seq.tail, isCube)
    else {
      val steps = seq.takeWhile(_.isDigit).mkString.toInt
      move(forward(p, steps, isCube), seq.dropWhile(_.isDigit), isCube)
    }
  }

  def printPath() = {
    for (row <- grid.indices) {
      for (col <- grid.head.indices) {
        val pp = path.find(p => p.row == row && p.col == col)
        if (pp.nonEmpty)
          print(pp.get.dir match {
            case 0 => ">"
            case 1 => "v"
            case 2 => "<"
            case 3 => "^"
          })
        else
          print(grid(row)(col))
      }
      println()
    }
  }


  val finish1 = move(P(0, grid.head.indexWhere(_ != ' '), 0), seq, false)
  //printPath()
  val part1 = (finish1.row + 1) * 1000 + (finish1.col + 1) * 4 + finish1.dir
  println(part1)

  def findCubeOpposite(p : P): P = {
    if (p.row == 0 && p.col >= 50 && p.col < 100 && p.dir == 3) { // 4-6
      p.copy(row = 150 + p.col - 50, col = 0, dir = 0)
    } else if (p.row >= 150 && p.col == 0 && p.dir == 2) { // 6-4
      p.copy(row = 0, col = 50 + p.row - 150, dir = 1)
    } else if (p.row == 0 && p.col >= 100 && p.dir == 3) { // 3-6
      p.copy(row = 199, col = p.col - 100, dir = 3)
    } else if (p.row == 199 && p.dir == 1) { // 6-3
      p.copy(row = 0, col = 100 + p.col, dir = 1)
    } else if (p.row < 50 && p.col == 50 && p.dir == 2) { // 4-5
      p.copy(row = 149 - p.row, col = 0, dir = 0)
    } else if (p.row >= 100 && p.row < 150 && p.col == 0 && p.dir == 2) { // 5-4
      p.copy(row = 49 - p.row + 100, col = 50, dir = 0)
    } else if (p.row >= 50 && p.row < 100 && p.col == 50 && p.dir == 2) { // 1-5
      p.copy(row = 100, col = p.row - 50, dir = 1)
    } else if (p.row == 100 && p.col < 50 && p.dir == 3) { // 5-1
      p.copy(row = 50 + p.col, col = 50, dir = 0)
    } else if (p.row == 49 && p.col >= 100 && p.dir == 1) { // 3-1
      p.copy(row = 50 + p.col - 100, col = 99, dir = 2)
    } else if (p.row >= 50 && p.row < 100 && p.col == 99 && p.dir == 0) { // 1-3
      p.copy(row = 49, col = 100 + p.row - 50, dir = 3)
    } else if (p.row < 50 && p.col == 149 && p.dir == 0) { // 3-2
      p.copy(row = 149 - p.row, col = 99, dir = 2)
    } else if (p.row >= 100 && p.row < 150 && p.col == 99 && p.dir == 0) { // 2-3
      p.copy(row = 49 - p.row + 100, col = 149, dir = 2)
    } else if (p.row == 149 && p.col >= 50 && p.dir == 1) { // 2-6
      p.copy(row = 150 + p.col - 50, col = 49, dir = 2)
    } else if (p.row >= 150 && p.col == 49 && p.dir == 0) { // 6-2
      p.copy(row = 149, col = 50 + p.row - 150, dir = 3)
    } else {
      throw new Exception("Trying to find opposite for " + p)
    }
  }


  val finish2 = move(P(0, grid.head.indexWhere(_ != ' '), 0), seq, true)
  //printPath()
  val part2 = (finish2.row + 1) * 1000 + (finish2.col + 1) * 4 + finish2.dir
  println(part2)

}

