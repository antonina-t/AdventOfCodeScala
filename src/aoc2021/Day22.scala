package aoc2021

import scala.io.Source
import scala.util.Using

object Day22 extends App {
  val input = Using(Source.fromFile("input/2021/22.txt")) {
    _.getLines.toSeq
  }.get

  val line = raw"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r

  val grid = Array.ofDim[Boolean](101, 101, 101)
  input.take(20).foreach {
    case line(onOff, x1s, x2s, y1s, y2s, z1s, z2s) =>
      val x1 = x1s.toInt + 50
      val x2 = x2s.toInt + 50
      val y1 = y1s.toInt + 50
      val y2 = y2s.toInt + 50
      val z1 = z1s.toInt + 50
      val z2 = z2s.toInt + 50
      for (x <- x1 to x2; y <- y1 to y2; z <- z1 to z2 if (x >= 0 && x <= 100 && y >= 0 && y <= 100 && z >= 0 && z <= 100)) {
        grid(x)(y)(z) = onOff == "on"
      }
  }

  val part1 = grid.map(_.map(_.count(identity)).sum).sum
  println(part1)

  val in = input.map {
    case line(onOff, x1s, x2s, y1s, y2s, z1s, z2s) =>
      val x1 = x1s.toInt
      val x2 = x2s.toInt
      val y1 = y1s.toInt
      val y2 = y2s.toInt
      val z1 = z1s.toInt
      val z2 = z2s.toInt
      (onOff == "on", Cube(x1, x2, y1, y2, z1, z2))
  }

  case class Cube(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) {
    val vertices = Seq((x1, y1, z1), (x1, y1, z2), (x1, y2, z1), (x1, y2, z2),
      (x2, y1, z1), (x2, y1, z2), (x2, y2, z1), (x2, y2, z2))
    val volume: Long = 1L * (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)
  }

  def areDisjoint(c1: Cube, c2: Cube) = {
    c2.x2 < c1.x1 || c2.x1 > c1.x2 ||
      c2.y2 < c1.y1 || c2.y1 > c1.y2 ||
      c2.z2 < c1.z1 || c2.z1 > c1.z2
  }

  val cubes = in.foldLeft(Seq.empty[Cube])((cubes, cubeData) => {
    val (on, c1) = cubeData
    val newCubes = cubes.flatMap(c2 => if (areDisjoint(c1, c2)) Seq(c2) else {
      val p1 = if (c1.x1 > c2.x1) Some(Cube(c2.x1, c1.x1 - 1, c2.y1, c2.y2, c2.z1, c2.z2)) else None
      val p2 = if (c1.x2 < c2.x2) Some(Cube(c1.x2 + 1, c2.x2, c2.y1, c2.y2, c2.z1, c2.z2)) else None
      val p3 = if (c1.y1 > c2.y1) Some(Cube(Math.max(c1.x1, c2.x1), Math.min(c1.x2, c2.x2), c2.y1, c1.y1 - 1, c2.z1, c2.z2)) else None
      val p4 = if (c1.y2 < c2.y2) Some(Cube(Math.max(c1.x1, c2.x1), Math.min(c1.x2, c2.x2), c1.y2 + 1, c2.y2, c2.z1, c2.z2)) else None
      val p5 = if (c1.z1 > c2.z1) Some(Cube(Math.max(c1.x1, c2.x1), Math.min(c1.x2, c2.x2), Math.max(c1.y1, c2.y1), Math.min(c1.y2, c2.y2), c2.z1, c1.z1 - 1)) else None
      val p6 = if (c1.z2 < c2.z2) Some(Cube(Math.max(c1.x1, c2.x1), Math.min(c1.x2, c2.x2), Math.max(c1.y1, c2.y1), Math.min(c1.y2, c2.y2), c1.z2 + 1, c2.z2)) else None
      Seq(p1, p2, p3, p4, p5, p6).flatten
    })
    if (on) newCubes :+ c1 else newCubes
  })

  val part2 = cubes.map(_.volume).sum
  println(part2)

}
