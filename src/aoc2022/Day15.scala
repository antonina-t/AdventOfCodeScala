package aoc2022

import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val pattern = raw"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)".r

  val input = Using(Source.fromFile("input/2022/15.txt")) {
    _.getLines().toSeq
  }.get.map {
    case pattern(sX, sY, bX, bY) => (sX.toInt, sY.toInt, bX.toInt, bY.toInt)
  }

  val max = 4000000

  val sensors = input.map {
    case (sX, sY, bX, bY) =>
      val r = Math.abs(bY - sY) + Math.abs(bX - sX)
      D(P(sX, sY), r)
  }

  val myY = 2000000

  val myYBeacons = input.collect {
    case (_, _, bX, bY) if bY == myY => bX
  }.toSet

    val part1 = sensors.foldLeft(Seq.empty[Int]) {
      case (points, sensor) =>
        val minY = sensor.center.y - sensor.radius
        val maxY = sensor.center.y + sensor.radius
        if (myY >= minY && myY <= maxY) {
          val distanceToMyY = Math.abs(myY - sensor.center.y)
          val delta = sensor.radius - distanceToMyY
          points ++ ((sensor.center.x - delta) to (sensor.center.x + delta))
        } else
          points
    }.toSet -- myYBeacons
    println(part1.size)

  case class P(x: Int, y: Int) // point

  case class L(a: P, b: P) // line

  case class D(center: P, radius: Int) // diamond with "radius"

  def contains(p: P, d: D) = {
    Math.abs(p.x - d.center.x) + Math.abs(p.y - d.center.y) <= d.radius
  }

  def linesAfterOverlapWithSensor(line: L, sensor: D): Seq[L] = {
    val a = line.a
    val b = line.b
    val sX = sensor.center.x
    val sY = sensor.center.y
    val distance = sensor.radius

    val xDir = if (line.b.x > line.a.x) 1 else -1
    val myY = a.y - xDir * a.x
    val minY = sY - xDir * sX - distance
    val maxY = sY - xDir * sX + distance

    if (myY > maxY || myY < minY || (!contains(a, sensor) && !contains(b, sensor) && ((a.x < sX && b.x < sX) || (a.x > sX && b.x > sX))))
      Seq(line)
    else {
      if (contains(a, sensor) && contains(b, sensor) || (!contains(a, sensor) && !contains(b, sensor) && (a.x < sX && b.x < sX) || (a.x > sX && b.x > sX)))
        Seq.empty
      else {
        val newBOpt = ((a.x to b.x by xDir) zip (a.y to b.y)).takeWhile(p => Math.abs(p._1 - sX) + Math.abs(p._2 - sY) > distance).lastOption
        val newAOpt = ((b.x to a.x by -xDir) zip (b.y to a.y by -1)).takeWhile(p => Math.abs(p._1 - sX) + Math.abs(p._2 - sY) > distance).lastOption

        val top = newBOpt.map(newB => L(a, P(newB._1, newB._2)))
        val bottom = newAOpt.map(newA => L(P(newA._1, newA._2), b))

        Seq(top, bottom).flatten
      }
    }
  }

  def getBorder(add: Int) = sensors.map {
    d => {
      val top = d.center.copy(y = d.center.y - d.radius - add)
      val right = d.center.copy(x = d.center.x + d.radius + add)
      val bottom = d.center.copy(y = d.center.y + d.radius + add)
      val left = d.center.copy(x = d.center.x - d.radius - add)
      val topRight = L(top, right)
      val bottomLeft = L(left, bottom)
      val topLeft = L(top, left)
      val bottomRight = L(right, bottom)
      d -> Seq(trim(topRight), trim(bottomLeft), trim(topLeft), trim(bottomRight)).flatten
    }
  }

  def printLines(lines: Seq[L]) = {
    val ps = lines.foldLeft(Set.empty[P]) {
      case (ps, line) =>
        val (a, b) = (line.a, line.b)
        if (b.x > a.x) {
          ps ++ ((a.x to b.x) zip (a.y to b.y)).map(p => P(p._1, p._2)).toSet
        } else {
          ps ++ ((a.x to b.x).by(-1) zip (a.y to b.y)).map(p => P(p._1, p._2)).toSet
        }
    }

    for (y <- 0 to max) {
      print(y % 10)
      for (x <- 0 to max) {
        print(if (ps.contains(P(x, y))) "#" else ".")
      }
      println()
    }
  }

  def trim(line: L): Option[L] = {
    val (a, b) = (line.a, line.b)
    if (b.x > a.x) {
      val myY = a.y - a.x
      if (myY < -max || myY > max)
        None
      else if (a.y > max || a.x > max || b.y < 0 || b.x < 0)
        None
      else {
        val newP1 = if (a.x < 0) P(0, a.y - a.x) else a
        val newNewP1 = if (newP1.y < 0) P(newP1.x - newP1.y, 0) else newP1
        val newP2 = if (b.x > max) P(max, b.y - (b.x - max)) else b
        val newNewP2 = if (newP2.y > max) P(newP2.x - (newP2.y - max), max) else newP2
        Some(L(newNewP1, newNewP2))
      }
    } else {
      val myY = a.y + a.x
      if (myY < 0 || myY > max * 2)
        None
      else if (a.x < 0 || a.y > max || b.y < 0 || b.x > max)
        None
      else {
        val newP1 = if (a.x > max) P(max, a.y + (a.x - max)) else a
        val newNewP1 = if (newP1.y < 0) P(newP1.x + newP1.y, 0) else newP1
        val newP2 = if (b.x < 0) P(0, b.y + b.x) else b
        val newNewP2 = if (newP2.y > max) P(newP2.x + (newP2.y - max), max) else newP2
        Some(L(newNewP1, newNewP2))
      }
    }
  }

  val linesLeft = getBorder(1).map {
    case (d, border) =>
      //println(d)
      //printLines(border)
      val left = sensors.filterNot(_ == d).foldLeft(border) {
        case (borderLines, sensor) =>
          //println("shadow by " + sensor)
          //printLines(getBorder(0).find(_._1 == sensor).get._2 ++ lines)
          borderLines.flatMap(line => linesAfterOverlapWithSensor(line, sensor))
      }
      d -> left
  }.filter(_._2.nonEmpty)

  val distressPoint = linesLeft.head._2.head.a
  val part2 = 4000000L * distressPoint.x + distressPoint.y
  println(part2)

}

