package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val input = Using(Source.fromFile("input/2022/17.txt")) {
    _.getLines().toSeq
  }.get.head

  case class P(x: Int, y: Int)

  def getShape(tetris: Set[P], i: Int, command: Int): (Set[P], Int) = {
    val top = if (tetris.isEmpty) 1 else tetris.maxBy(_.y).y + 1

    val shapeStart = i % 5 match {
      case 0 => Set(P(2, top + 3), P(3, top + 3), P(4, top + 3), P(5, top + 3))
      case 1 => Set(P(3, top + 3), P(2, top + 4), P(3, top + 4), P(4, top + 4), P(3, top + 5))
      case 2 => Set(P(2, top + 3), P(3, top + 3), P(4, top + 3), P(4, top + 4), P(4, top + 5))
      case 3 => Set(P(2, top + 3), P(2, top + 4), P(2, top + 5), P(2, top + 6))
      case 4 => Set(P(2, top + 3), P(3, top + 3), P(2, top + 4), P(3, top + 4))
    }

    def moveSide(side: Char, shape: Set[P]): Set[P] = {
      side match {
        case '>' =>
          val newShape = shape.map(p => p.copy(x = p.x + 1))
          if (newShape.exists(p => p.x >= 7 || tetris.contains(p))) shape else newShape
        case '<' =>
          val newShape = shape.map(p => p.copy(x = p.x - 1))
          if (newShape.exists(p => p.x < 0 || tetris.contains(p))) shape else newShape
        case _ => throw new RuntimeException("Unsupported direction")
      }
    }

    def moveDown(shape: Set[P]): Set[P] = {
      val newShape = shape.map(p => p.copy(y = p.y - 1))
      if (newShape.exists(p => p.y <= 0 || tetris.contains(p)))
        shape
      else
        newShape
    }

    @tailrec
    def move(shape: Set[P], command: Int): (Set[P], Int) = {
      val side = moveSide(input.charAt(command), shape)
      val down = moveDown(side)
      if (down == side) {
        (down, (command + 1) % input.length)
      } else {
        move(down, (command + 1) % input.length)
      }
    }

    move(shapeStart, command)
  }

  def printTetris(tetris: Set[P]): Unit = {
    if (tetris.nonEmpty) {
      val maxY = tetris.maxBy(_.y).y
      val minY = maxY - 50

      for (y <- maxY to minY by -1) {
        for (x <- 0 until 7) {
          print(if (tetris.contains(P(x, y))) "#" else ".")
        }
        println()
      }
    }
  }

  val part1 = (0 until 2022).foldLeft((Set.empty[P], 0)) {
    case ((tetris, command), i) =>
      val (newShape, newCommand) = getShape(tetris, i, command)
      val newTetris = tetris ++ newShape
      (newTetris, newCommand)
  }._1.maxBy(_.y).y

  println(part1)


  def cleanBottom(tetris: Set[P]) = {
    if (tetris.isEmpty)
      tetris
    else {
      val maxY = tetris.maxBy(_.y).y
      val lineOpt = (maxY to 1 by -1).find(y => (0 until 7).forall(x => tetris.contains(P(x, y))))
      lineOpt.map(line => tetris.filter(_.y >= line)).getOrElse(tetris)
    }
  }

  def run(fromCycle: Int, toCycle: Int, tetris: Set[P], command: Int) = {
    (fromCycle until toCycle).scanLeft((tetris, command, Set.empty[(Int,Int)], fromCycle, Set.empty[P], command)) {
      case ((tetris, command, positions, _, tetrisAtCycleStart, commandAtCycleStart), i) =>
        val newCycle = positions.contains((command, i % 5))
        val newCycleStartsAt = if (newCycle) i else fromCycle
        val cleanedTetris = cleanBottom(tetris)
        val (newShape, newCommand) = getShape(cleanedTetris, i, command)
        val newTetris = cleanedTetris ++ newShape
        (newTetris, newCommand, positions ++ Set((command, i % 5)), newCycleStartsAt, if (newCycle) tetris else tetrisAtCycleStart, if (newCycle) command else commandAtCycleStart)
    }
  }

  def findHeight(): Long = {
    val (_, _, _, cycleStartsAt, tetrisAtCycleStart, commandAtCycleStart) =
      run(0, 10000, Set.empty[P], 0).dropWhile(_._4 == 0).head

    val (_, _, _, cycleEndsAt, tetrisAtCycleEnd, _) =
      run(cycleStartsAt, 10000, tetrisAtCycleStart, commandAtCycleStart).dropWhile(_._4 == cycleStartsAt).head

    val topAtCycleStart = tetrisAtCycleStart.maxBy(_.y).y
    val topAtCycleEnd = tetrisAtCycleEnd.maxBy(_.y).y
    val cycleLength = cycleEndsAt - cycleStartsAt
    val cycleHeight = topAtCycleEnd - topAtCycleStart

    val cycleRepetitions = (1000000000000L - cycleStartsAt) / cycleLength
    val extraRuns = (1000000000000L - cycleStartsAt - (cycleRepetitions * cycleLength)).toInt

    val (tetrisAfterExtraRuns, _, _, _, _, _) =
      run(cycleStartsAt, cycleStartsAt + extraRuns, tetrisAtCycleStart, commandAtCycleStart).last

    val extraHeight = tetrisAfterExtraRuns.maxBy(_.y).y - topAtCycleStart

    topAtCycleStart + (cycleRepetitions * cycleHeight) + extraHeight
  }

  val part2 = findHeight()
  println(part2)

}

