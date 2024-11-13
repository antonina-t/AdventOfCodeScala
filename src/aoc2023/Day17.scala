package aoc2023

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val input = Using(Source.fromFile("input/2023/17.txt")) {
    _.getLines().toSeq
  }.get

  val Top = 0
  val Right = 1
  val Bottom = 2
  val Left = 3

  case class Node(row: Int, col: Int, dirs: Set[Int])

  def distances(distanceSoFar: Int, nodes: Seq[Node]) = {
    val distances = nodes.scanLeft(distanceSoFar)((acc, node) => acc + input(node.row)(node.col).asDigit)
    nodes.zip(distances.tail).toSet
  }

  def search(visited: Set[Node], next: Set[(Node, Int)], minSteps: Int, maxSteps: Int): Int = {
    val (node, distance) = next.minBy(_._2)
    if (node.row == input.length - 1 && node.col == input.head.length - 1)
      distance
    else {
      val top = if (node.dirs.contains(Top)) {
        val nodes = (minSteps to maxSteps).map(i => node.row - i).filter(input.indices.contains).map(row => Node(row, node.col, Set(Left, Right)))
        distances(distance, nodes)
      } else Set.empty[(Node, Int)]

      val bottom = if (node.dirs.contains(Bottom)) {
        val nodes = (minSteps to maxSteps).map(i => node.row + i).filter(input.indices.contains).map(row => Node(row, node.col, Set(Left, Right)))
        distances(distance, nodes)
      } else Set.empty[(Node, Int)]

      val left = if (node.dirs.contains(Left)) {
        val nodes = (minSteps to maxSteps).map(i => node.col - i).filter(input.head.indices.contains).map(col => Node(node.row, col, Set(Top, Bottom)))
        distances(distance, nodes)
      } else Set.empty[(Node, Int)]

      val right = if (node.dirs.contains(Right)) {
        val nodes = (minSteps to maxSteps).map(i => node.col + i).filter(input.head.indices.contains).map(col => Node(node.row, col, Set(Top, Bottom)))
        distances(distance, nodes)
      } else Set.empty[(Node, Int)]

      val neighbours = (top ++ right ++ bottom ++ left).filterNot(value => visited.contains(value._1))

      search(visited + node, (next ++ neighbours).filterNot(_._1 == node), minSteps, maxSteps)
    }
  }

  val part1 = search(Set.empty, Set((Node(0, 0, Set(Right, Bottom)), 0)), 1, 3)
  println(part1)

  val part2 = search(Set.empty, Set((Node(0, 0, Set(Right, Bottom)), 0)), 4, 10)
  println(part2)

}

