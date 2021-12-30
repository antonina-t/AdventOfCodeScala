package aoc2021

import scala.io.Source
import scala.util.Using

object Day23 extends App {
  val input = Using(Source.fromFile("input/2021/23.txt")) {
    _.getLines.toSeq
  }.get

  def isInHall(y: Int) = y == 1

  def isInRightRoom(c: Char, x: Int, y: Int, state: Array[String]): Boolean =
    ((y + 1) to 5).forall(someY => state(someY)(x) == c) && x == getHomeX(c)

  def getHomeX(c: Char) = c match {
    case 'A' => 3
    case 'B' => 5
    case 'C' => 7
    case 'D' => 9
  }

  def getScoreUnit(c: Char) = c match {
    case 'A' => 1
    case 'B' => 10
    case 'C' => 100
    case 'D' => 1000
  }

  def getHallSteps(c: Char, x: Int, y: Int, state: Array[String], score: Int): Map[String, Int] = {
    if ((2 until y).exists(state(_)(x) != '.')) Map.empty
    else {
      val hallStates = Seq(1, 2, 4, 6, 8, 10, 11)
        .filter(otherX => (Math.min(x, otherX) to Math.max(x, otherX)).forall(pathX => state(1)(pathX) == '.'))
        .map(otherX => {
          val newState = state.indices.map(newY => state(newY).indices.map(newX =>
            if (newY == 1 && newX == otherX)
              c
            else if (newY == y && newX == x)
              '.'
            else
              state(newY)(newX)
          ).mkString).mkString("\n")
          val newScore = score + getScoreUnit(c) * (y - 1 + Math.abs(otherX - x))
          (newState, newScore)
        }
        ).toMap
      hallStates
    }
  }

  def getRoomStep(c: Char, x: Int, y: Int, state: Array[String], score: Int): Map[String, Int] = {
    val homeX = getHomeX(c)
    val canMoveInHall = (Math.min(x, homeX) to Math.max(x, homeX)).forall(pathX => pathX == x || state(1)(pathX) == '.')
    val canMoveHome = (2 to 5).forall(otherY => state(otherY)(homeX) == '.' || state(otherY)(homeX) == c)
    if (canMoveInHall && canMoveHome) {
      val homeY = (5 to 2 by -1).dropWhile(state(_)(homeX) == c).head
      val newState = state.indices.map(newY => state(newY).indices.map(newX =>
        if (newY == homeY && newX == homeX)
          c
        else if (newY == y && newX == x)
          '.'
        else
          state(newY)(newX)
      ).mkString).mkString("\n")
      val newScore = score + getScoreUnit(c) * (Math.abs(x - homeX) + Math.abs(homeY - y))
      Map(newState -> newScore)
    } else Map.empty
  }

  def getNextSteps(stateScore: (String, Int)): Map[String, Int] = {
    val (state, score) = stateScore
    val stateArr = state.split("\n")
    val letterIndices = stateArr.indices.flatMap(y => stateArr(y).indices.filter(stateArr(y)(_).isLetter).map((_, y)))
    letterIndices.map { case (x, y) =>
      val c = stateArr(y)(x)
      if (isInHall(y)) {
        getRoomStep(c, x, y, stateArr, score)
      } else if (!isInRightRoom(c, x, y, stateArr)) {
        getHallSteps(c, x, y, stateArr, score)
      } else
        Map.empty[String, Int]
    }.reduce(_ ++ _)
  }


  @scala.annotation.tailrec
  def solve(steps: Set[String], mem: Map[String, Int], visited: Set[String]): Int = {
    println("Steps: " + steps.size)
    if (steps.isEmpty) mem(end.mkString("\n"))
    else {
      val nextSteps = steps
        .map(s => (s, mem(s))).map(getNextSteps)
        .reduce((map1, map2) => map1.keySet.union(map2.keySet).map(k => (k, Math.min(map1.getOrElse(k, Integer.MAX_VALUE), map2.getOrElse(k, Integer.MAX_VALUE)))).toMap)
      solve(nextSteps.keySet.diff(visited), mem ++ nextSteps, visited ++ nextSteps.keySet)
    }
  }

  // Solved by hand
  val part1 = 15322
  println(part1)

  val end = Seq(
    "#############",
    "#...........#",
    "###A#B#C#D###",
    "  #A#B#C#D#",
    "  #A#B#C#D#",
    "  #A#B#C#D#",
    "  #########")

  // Slow, but works
  val part2 = solve(Set(input.mkString("\n")), Map(input.mkString("\n") -> 0), Set(input.mkString("\n")))
  println(part2)

}
