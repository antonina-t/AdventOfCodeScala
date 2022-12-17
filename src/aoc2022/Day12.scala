package aoc2022

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input = Using(Source.fromFile("input/2022/12.txt")) {
    _.getLines().toSeq
  }.get.map(_.toArray).toArray

  case class P(x: Int, y: Int)

  def elDiff(p1: P, p2: P) = {
    val el1 = if (input(p1.y)(p1.x) == 'S') 'a' else if (input(p1.y)(p1.x) == 'E') 'z' else input(p1.y)(p1.x)
    val el2 = if (input(p2.y)(p2.x) == 'S') 'a' else if (input(p2.y)(p2.x) == 'E') 'z' else input(p2.y)(p2.x)
    el2 - el1
  }

  def neighbors(forP: P, visited: Set[P]) = {
    Seq(P(forP.x - 1, forP.y), P(forP.x + 1, forP.y), P(forP.x, forP.y - 1), P(forP.x, forP.y + 1))
      .filter(p => input.indices.contains(p.y) && input(0).indices.contains(p.x))
      .filter(p => !visited.contains(p))
      .filter(p => elDiff(forP, p) <= 1)
  }

  def findDistance(distance: Int, these: Seq[P], visited: Set[P], max: Int): Int = {
    if (distance >= max)
      distance
    else if (these.exists(p => input(p.y)(p.x) == 'E'))
      distance
    else {
      findDistance(distance + 1, these.flatMap(p => neighbors(p, visited)).distinct, visited ++ these, max)
    }
  }

  val startY = input.indexWhere(_.contains('S'))
  val startX = input(startY).indexOf('S')
  val part1 = findDistance(0, Seq(P(startX, startY)), Set.empty[P], 1000)
  println(part1)

  val part2 = (for (y <- input.indices; x <- input(0).indices) yield P(x,y)).filter(p => input(p.y)(p.x) == 'a')
    .map(p => findDistance(0, Seq(p), Set.empty[P], 1000)).min
  println(part2)
}

