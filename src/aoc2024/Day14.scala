package aoc2024

import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val input = Using(Source.fromFile("input/2024/14.txt")) {
    _.getLines().toSeq
  }.get

  //p=44,70 v=-47,36

  val pattern = raw"p=(\d+),(\d+) v=(-?\d+),(-?\d+)".r

  val robots = input.map(line => {
    line match {
      case pattern(x, y, vx, vy) => Robot(P(y.toInt, x.toInt), P(vy.toInt, vx.toInt))
    }
  })

  val width = 101
  val height = 103

  def walk(robots: Set[Robot], steps: Int, visited: Set[Set[Robot]]): Int = {
    //9987, 9886, 9785
    if (visited.contains(robots))
      println(steps, "REPEAT!!!")

    if ((11000 - 13 - steps) % 101 == 0) {
      println("steps: " + steps)
      for (row <- 0 until height; col <- 0 until width) {
        val robotsCount = robots.count(_.p == P(row, col))
        print(if (robotsCount > 0) '*' else '.')
        if (col == width - 1)
          println()
      }
      Thread.sleep(500)
    }
    
    if (steps == 0) {
      val topL = robots.count(r => r.p.row < height / 2 && r.p.col < width / 2)
      val bottomL = robots.count(r => r.p.row > height / 2 && r.p.col < width / 2)
      val topR = robots.count(r => r.p.row < height / 2 && r.p.col > width / 2)
      val bottomR = robots.count(r => r.p.row > height / 2 && r.p.col > width / 2)
      topL * topR * bottomL * bottomR
    } else {
      val newRobots = robots.map(robot => {
        Robot(P(
          (robot.p.row + robot.v.row + height) % height,
          (robot.p.col + robot.v.col + width) % width,
        ), robot.v)
      })
      walk(newRobots, steps - 1, visited ++ Set(robots))
    }
  }


  val part2 = walk(robots.toSet, 11000, Set.empty)
  println(part2)

}

case class Robot(p: P, v: P)
