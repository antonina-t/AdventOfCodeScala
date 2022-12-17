package aoc2022

import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val input = Using(Source.fromFile("input/2022/14.txt")) {
    _.getLines().toSeq
  }.get

  case class P(x: Int, y: Int)

  val rock = input.foldLeft(Set.empty[P]){
    case (ps, line) =>
      val corners = line.split(" -> ").map(_.split(",")).map(arr => P(arr(0).toInt, arr(1).toInt))
      ps ++ corners.foldLeft(Seq(corners.head)) {
        case (path, corner) =>
          val lastCorner = path.last
          val dx = corner.x - lastCorner.x
          val dy = corner.y - lastCorner.y
          val dxPs = if (dx == 0) Nil else (0 to dx by(if (dx > 0) 1 else -1)).map(d => P(lastCorner.x + d, lastCorner.y))
          val dyPs = if (dy == 0) Nil else (0 to dy by(if (dy > 0) 1 else -1)).map(d => P(lastCorner.x, lastCorner.y + d))
          path ++ dxPs ++ dyPs
      }.toSet
  }

  val minX = rock.minBy(_.x).x
  val maxX = rock.maxBy(_.x).x
  val minY = rock.minBy(_.y).y
  val maxY = rock.maxBy(_.y).y + 2

  println(minX, maxX, minY, maxY)

  def printSand(sand: Set[P]): Unit = {
    println()
    for (y <- 0 to maxY) {
      for (x <- minX to maxX)
        print(
          if (rock.contains(P(x, y)))
            '#'
          else if (sand.contains(P(x,y)))
            'o'
          else '.')
      println()
    }
  }

  val sandStart = P(500,0)

  def isFree(p: P, sand: Set[P]) = !rock.contains(p) && ! sand.contains(p) && p.y != maxY

  def addOneSand(allSand: Set[P], p: P): P = {
    if (p.y >= maxY)
      p
    else {
      val yDown = (p.y to maxY).takeWhile(newY => isFree(p.copy(y = newY), allSand)).lastOption

      if (yDown.isEmpty)
        p.copy(y = maxY)
      else {
        val pDown = P(p.x, yDown.get)
        val pLeft = p.copy(x = p.x - 1, y = pDown.y + 1)
        val pRight = p.copy(x = p.x + 1, y = pDown.y + 1)
        if (isFree(pLeft, allSand))
          addOneSand(allSand, pLeft)
        else if (isFree(pRight, allSand))
          addOneSand(allSand, pRight)
        else
          pDown
      }
    }
  }

  def addSand(allSand: Set[P]): Set[P] = {
    val newSand = addOneSand(allSand, sandStart)
    if (newSand.y >= maxY)
      allSand
    else addSand(allSand ++ Set(newSand))
  }


  val part2 = addSand(Set.empty).size
  println(part2)


}

