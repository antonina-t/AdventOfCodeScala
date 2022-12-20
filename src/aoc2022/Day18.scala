package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18 extends App {

  val input = Using(Source.fromFile("input/2022/18.txt")) {
    _.getLines().toSeq
  }.get

  case class C(x:Int,y:Int,z:Int)

  val lava = input.map(_.split(",").map(_.toInt)).map(arr => C(arr(0),arr(1),arr(2)))

  def neighbors(c: C) = Seq(
    c.copy(x = c.x - 1),
    c.copy(x = c.x + 1),
    c.copy(y = c.y - 1),
    c.copy(y = c.y + 1),
    c.copy(z = c.z - 1),
    c.copy(z = c.z + 1),
  )

  val part1 = lava.map(c => {
    neighbors(c).count(c => !lava.contains(c))
  }).sum

  println("Part 1: " + part1)

  val air = lava.flatMap(c => {
    neighbors(c).filter(c => !lava.contains(c))
  }).toSet

  val reachMe = C(lava.minBy(_.x).x - 1,lava.minBy(_.y).y,lava.minBy(_.z).z)

  @tailrec
  def isEnclosed(cubes: Set[C], visited: Set[C]): Boolean = {
    if (cubes.isEmpty)
      true
    else if (cubes.contains(reachMe))
      false
    else {
      val next = cubes.flatMap(c => neighbors(c)).diff(lava.toSet).diff(visited ++ cubes)
      if (next.contains(reachMe)) {
        false
      } else {
        isEnclosed(next, visited ++ cubes)
      }
    }
  }

  var i = 0.0
  println("Computing free air...")
  val freeAir = air.filter(c => {
    println((i / air.size * 100).toInt + "%")
    i = i + 1
    !isEnclosed(Set(c), Set.empty)
  })

  val part2 = lava.map(c => {
    neighbors(c).count(c => freeAir.contains(c))
  }).sum

  println("Part 2: " + part2)

}

