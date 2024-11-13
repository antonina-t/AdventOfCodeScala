package aoc2022

import scala.io.Source
import scala.util.Using

object Day24 extends App {

  val input = Using(Source.fromFile("input/2022/24.txt")) {
    _.getLines().toSeq
  }.get

  case class P(x: Int, y: Int)

  val blizzards = input.zipWithIndex.flatMap {
    case (line, y) => {
      line.zipWithIndex.map {
        case (c, x) =>
          c match {
            case '.' => None
            case '#' => None
            case dir => Some(P(x, y) -> dir)
          }
      }
    }
  }.flatten

  def getNextBlizzards(blizzards: Seq[(P, Char)]) = {
    blizzards.map {
      case (P(x, y) -> dir) =>
        dir match {
          case '>' => P(if (x == input.head.length - 2) 1 else x + 1, y) -> dir
          case '<' => P(if (x == 1) input.head.length - 2 else x - 1, y) -> dir
          case 'v' => P(x, if (y == input.length - 2) 1 else y + 1) -> dir
          case '^' => P(x, if (y == 1) input.length - 2 else y - 1) -> dir
        }
    }
  }

  def printBlizzards(blizzards: Seq[(P, Char)]) = {
    for (y <- input.indices) {
      for (x <- input.head.indices) {
        val blizzCount = blizzards.count(_._1 == P(x, y))
        if (blizzCount > 1)
          print(blizzCount)
        else if (blizzCount == 1)
          print(blizzards.find(_._1 == P(x, y)).get._2)
        else
          print(input(y)(x))
      }
      println()
    }
    println()

  }

  def bfs(length: Int, goals: Seq[P], next: Seq[P], blizzards: Seq[(P, Char)]): Int = {
    //printBlizzards(blizzards)
    //println(length)
    val newBlizzards = getNextBlizzards(blizzards)
    val newBlizzardPos = newBlizzards.map(_._1).toSet
    val newNext = next
      .flatMap(p => Seq(p, p.copy(x = p.x - 1), p.copy(x = p.x + 1), p.copy(y = p.y - 1), p.copy(y = p.y + 1)))
      .distinct
      .filter(p => !newBlizzardPos.contains(p))
      .filter(p => input.indices.contains(p.y) && input.head.indices.contains(p.x) && input(p.y)(p.x) != '#')
    if (newNext.contains(goals.head)) {
      if (goals.length == 1)
        length + 1
      else
        bfs(length + 1, goals.tail, Seq(goals.head), newBlizzards)
    } else
      bfs(length + 1, goals, newNext, newBlizzards)
  }

  val start = P(1, 0)
  val finish = P(input.head.length - 2, input.length - 1)

  val part1 = bfs(0, Seq(finish), Seq(start), blizzards)
  println(part1)

  val part2 = bfs(0, Seq(finish, start, finish), Seq(start), blizzards)
  println(part2)
}

