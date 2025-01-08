package aoc2024

import scala.io.Source
import scala.util.Using

object Day6 extends App {

  val input = Using(Source.fromFile("input/2024/6.txt")) {
    _.getLines().toSeq
  }.get

  def walk(visited: Set[(Int, Int)], pos: (Int, Int), dir: Int): Set[(Int, Int)] = {
    if (!input.indices.contains(pos._1) || !input.head.indices.contains(pos._2))
      visited
    else {
      val nextPos = getNext(pos, dir)
      if (isObstacle(nextPos))
        walk(visited ++ Set(pos), pos, (dir + 1) % 4)
      else
        walk(visited ++ Set(pos), nextPos, dir)
    }
  }

  def getNext(pos: (Int, Int), dir: Int): (Int, Int) = {
    dir match {
      case 0 => (pos._1 - 1, pos._2)
      case 1 => (pos._1, pos._2 + 1)
      case 2 => (pos._1 + 1, pos._2)
      case 3 => (pos._1, pos._2 - 1)
    }
  }

  def isObstacle(nextPos: (Int, Int)): Boolean = {
    input.indices.contains(nextPos._1) && input.head.indices.contains(nextPos._2) &&
      input(nextPos._1)(nextPos._2) == '#'
  }

  def isLoop(visited: Set[((Int, Int), Int)], pos: (Int, Int), dir: Int, input: Seq[String]): Boolean = {
    if (!input.indices.contains(pos._1) || !input.head.indices.contains(pos._2))
      false
    else if (visited.contains((pos, dir)))
      true
    else {
      val nextPos = getNext(pos, dir)
      if (isObstacle(nextPos))
        isLoop(visited ++ Set((pos, dir)), pos, (dir + 1) % 4, input)
      else
        isLoop(visited ++ Set((pos, dir)), nextPos, dir, input)
    }
  }

  val startPosRow = input.indexWhere(_.contains('^'))
  val startPosCol = input(startPosRow).indexOf('^')
  val part1 = walk(Set.empty, (startPosRow, startPosCol), 0).size
  println(part1)

  val candidates = for (row <- input.indices; col <- input(row).indices if input(row)(col) != '#' && input(row)(col) != '^') yield (row, col)
  val part2 = candidates.count {
    case (row, col) => isLoop(Set.empty, (startPosRow, startPosCol), 0, input.indices.map(r =>
      if (r == row)
        input(row).take(col) + "#" + input(row).drop(col + 1)
      else
        input(r)
    ))
  }
  println(part2)

}
