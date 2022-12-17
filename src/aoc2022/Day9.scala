package aoc2022

import scala.io.Source
import scala.util.Using

object Day9 extends App {

  val input = Using(Source.fromFile("input/2022/9.txt")) {
    _.getLines().toSeq
  }.get

  case class Point(x: Int, y:Int)

  def updateKnot(prevKnot: Point, thisKnot: Point) = {
    if (Math.abs(prevKnot.x - thisKnot.x) <= 1 && Math.abs(prevKnot.y - thisKnot.y) <= 1) thisKnot
    else {
      val newX = thisKnot.x + (if (thisKnot.x == prevKnot.x) 0 else if (prevKnot.x < thisKnot.x) -1 else 1)
      val newY = thisKnot.y + (if (thisKnot.y == prevKnot.y) 0 else if (prevKnot.y < thisKnot.y) -1 else 1)
      Point(newX, newY)
    }
  }

  val part1 = input.foldLeft((Point(0, 0), Point(0, 0), Set(Point(0, 0)))) {
    case ((head, tail, visited), line) =>
      val delta = line.charAt(0) match {
        case 'U' => (0, -1)
        case 'R' => (1, 0)
        case 'D' => (0, 1)
        case 'L' => (-1, 0)
      }
      val steps = line.substring(2).toInt
      (1 to steps).foldLeft((head, tail, visited)) {
        case ((head, tail, visited), _) =>
          val newHead = Point(head.x + delta._1, head.y + delta._2)
          val newTail = updateKnot(newHead, tail)
          (newHead, newTail, visited ++ Set(newTail))
      }
  }._3.size

  println(part1)

  def updateLongTail(head: Point, tail: Seq[Point]): Seq[Point] = tail.scanLeft(head)(updateKnot).tail

  val part2 = input.foldLeft((Point(0, 0), Seq.fill[Point](9)(Point(0,0)), Set(Point(0, 0)))) {
    case ((head, tail, visited), line) =>
      val delta = line.charAt(0) match {
        case 'U' => (0, -1)
        case 'R' => (1, 0)
        case 'D' => (0, 1)
        case 'L' => (-1, 0)
      }
      val steps = line.substring(2).toInt
      (1 to steps).foldLeft((head, tail, visited)) {
        case ((head, tail, visited), _) =>
          val newHead = Point(head.x + delta._1, head.y + delta._2)
          val newTail = updateLongTail(newHead, tail)
          (newHead, newTail, visited ++ Set(newTail.last))
      }
  }._3.size

  println(part2)

}

