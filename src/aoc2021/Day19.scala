package aoc2021

import scala.io.Source
import scala.util.Using

object Day19 extends App {
  val input = Using(Source.fromFile("input/2021/19.txt")) {
    _.getLines().toSeq
  }.get.foldLeft(Map.empty[Int, Set[(Int, Int, Int)]])((acc, line) => {
    if (line.startsWith("---")) acc + (acc.size -> Set.empty)
    else if (line.isEmpty) acc
    else {
      acc + (acc.size - 1 -> (acc(acc.size - 1) + (line.split(",").map(_.toInt) match {
        case Array(a, b, c) => (a, b, c)
      })))
    }
  })

  def getNeighbourOrigo(scanner1: Set[(Int, Int, Int)], scanner2: Set[(Int, Int, Int)]): Option[(Int, Int, Int)] = {
    val res = scanner1.flatMap {
      case p1@(x1, y1, z1) => scanner2.find {
        case (x2, y2, z2) =>
          val (dx, dy, dz) = (x1 - x2, y1 - y2, z1 - z2)
          scanner2.map { case (x, y, z) => (x + dx, y + dy, z + dz) }.intersect(scanner1).size >= 12
      }.map(p2 => (p1, p2))
    }.headOption
    res.map { case ((x1, y1, z1), (x2, y2, z2)) => (x1 - x2, y1 - y2, z1 - z2) }
  }

  def getOrientations(points: Set[(Int, Int, Int)]): Seq[Set[(Int, Int, Int)]] = {
    val half = Seq(
      points.map { case (x, y, z) => (x, y, z) },
      points.map { case (x, y, z) => (y, -x, z) },
      points.map { case (x, y, z) => (-x, -y, z) },
      points.map { case (x, y, z) => (-y, x, z) },

      points.map { case (x, y, z) => (x, -z, y) },
      points.map { case (x, y, z) => (-z, -x, y) },
      points.map { case (x, y, z) => (-x, z, y) },
      points.map { case (x, y, z) => (z, x, y) },

      points.map { case (x, y, z) => (y, z, x) },
      points.map { case (x, y, z) => (z, -y, x) },
      points.map { case (x, y, z) => (-y, -z, x) },
      points.map { case (x, y, z) => (-z, y, x) })
    half ++ half.map(_.map { case (x, y, z) => (y, x, -z) })
  }

  @scala.annotation.tailrec
  def explore(i: Int, scanners: Map[Int, Set[(Int, Int, Int)]], visited: Seq[(Int, (Int, Int, Int))]): (Map[Int, Set[(Int, Int, Int)]], Seq[(Int, (Int, Int, Int))]) = {
    //println(visited)
    if (scanners.size == visited.size) (scanners, visited)
    else {
      val neighbors = scanners.keySet.diff(visited.map(_._1).toSet).flatMap(j => {
        val s1 = scanners(visited(i)._1)
        val s2 = scanners(j)
        getOrientations(s2).flatMap(s => getNeighbourOrigo(s1, s).map((s, _, j)))
      })
      val updatedNei = neighbors.map { case (orientation, (x0, y0, z0), nextI) => nextI -> orientation.map { case (x, y, z) => (x + x0, y + y0, z + z0) } }
      explore(i + 1, scanners ++ updatedNei, visited ++ neighbors.map(n => (n._3, n._2)))
    }
  }

  val (beacons, scanners) = explore(0, input, Seq((0, (0, 0, 0))))

  val part1 = beacons.values.reduce(_ union _).size
  println(part1)

  val scannerPositions = scanners.map(_._2)
  val part2 = (for (s1 <- scannerPositions; s2 <- scannerPositions if s1 != s2)
    yield Math.abs(s1._1 - s2._1) + Math.abs(s1._2 - s2._2) + Math.abs(s1._3 - s2._3)).max
  println(part2)

}
