package aoc2021

import scala.io.Source
import scala.util.Using

object Day15 extends App {
  val input = Using(Source.fromFile("input/2021/15.txt")) {
    _.getLines.toSeq
  }.get.map(_.map(_ - '0').toArray).toArray

  val indices = for (y <- input.indices; x <- input(0).indices) yield (x, y)
  val maxX = input(0).indices.last
  val maxY = input.indices.last

  def dijkstra(visited: Map[(Int, Int), Int], unvisited: Map[(Int, Int), Int]): (Map[(Int, Int), Int], Map[(Int, Int), Int]) = {
    println(visited.size)
    if (unvisited.isEmpty) (visited, unvisited) else {
      val ((x, y), value) = unvisited.filter(_._2 != -1).minBy(_._2)
      val unvisitedNeighbors = Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).intersect(indices.toSet).diff(visited.keySet).map {
        case n@(nx, ny) =>
          if (unvisited(n) != -1 && unvisited(n) <= value + getValue(nx, ny))
            (n, unvisited(n))
          else
            (n, value + getValue(nx, ny))
      }.toMap
      dijkstra(visited ++ Map((x, y) -> value), unvisited.removed((x, y)) ++ unvisitedNeighbors)
    }
  }

  val part1 = dijkstra(Map.empty, Map((0, 0) -> 0) ++ indices.tail.map(_ -> -1))._1((maxX, maxY))
  println(part1)

  val indices2 = (for (y <- (0 until input.length * 5); x <- (0 until input(0).length * 5)) yield (x, y)).toSet
  val maxX2 = indices2.map(_._1).max
  val maxY2 = indices2.map(_._2).max

  def getValue(x: Int, y: Int): Int = {
    val v = input(y % input.length)(x % input(0).length) + y / input.length + x / input(0).length
    if (v <= 9) v else v - 9
  }

  @scala.annotation.tailrec
  def dijkstra2(visited: Map[(Int, Int), Int], unvisited: Map[(Int, Int), Int]): (Map[(Int, Int), Int], Map[(Int, Int), Int]) = {
    println(visited.size)
    if (unvisited.isEmpty) (visited, unvisited) else {
      val ((x, y), value) = unvisited.filter(_._2 != -1).minBy(_._2)
      val unvisitedNei = Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).intersect(indices2).diff(visited.keySet).map {
        case n@(nx, ny) =>
          if (unvisited(n) != -1 && unvisited(n) <= value + getValue(nx, ny))
            (n, unvisited(n))
          else
            (n, value + getValue(nx, ny))
      }.toMap
      dijkstra2(visited ++ Map((x, y) -> value), unvisited.removed((x, y)) ++ unvisitedNei)
    }
  }

  val part2 = dijkstra2(Map.empty, Map((0, 0) -> 0) ++ indices2.diff(Set((0, 0))).map(_ -> -1))._1((maxX2, maxY2))
  println(part2)

  /*
    @scala.annotation.tailrec
    def dijkstra3(scores: Array[Array[Int]], visited: collection.mutable.Set[(Int, Int)], unvisited: collection.mutable.Set[(Int, Int)]): Int = {
      println(visited.size)
      if (unvisited.isEmpty) scores(maxY2)(maxX2) else {
        val (x, y) = unvisited.filter{case (x,y) => scores(y)(x) != -1}.minBy{case (x,y) => scores(y)(x)}
        val score = scores(y)(x)
        Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).intersect(indices2).diff(visited).foreach {
          case (nx, ny) =>
            if (scores(ny)(nx) == -1 || scores(ny)(nx) > score + getValue(nx, ny))
              scores(ny)(nx) = score
        }
        visited.addOne((x,y))
        unvisited.remove((x,y))
        dijkstra3(scores, visited, unvisited)
      }
    }


    val scores = Array.fill(maxY2 + 1, maxX2 + 1)(-1)
    scores(0)(0) = 0
    val visited = collection.mutable.Set.empty[(Int, Int)]
    val unvisited = collection.mutable.Set.empty[(Int, Int)] ++ indices2
    val part2 = dijkstra3(scores, visited, unvisited)
    println(part2)
    */
}
