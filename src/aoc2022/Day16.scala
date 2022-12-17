package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val input = Using(Source.fromFile("input/2022/16.txt")) {
    _.getLines().toSeq
  }.get

  val pattern = raw"Valve (.*) has flow rate=(\d+); .* to valve[s]? (.*)".r

  case class Valve(id: String, flow: Int, next: Seq[String])

  val valves = input.foldLeft(Map.empty[String, Valve]) {
    case (map, pattern(valve, flow, next)) => map ++ Map(valve -> Valve(valve, flow.toInt, next.split(", ").toSeq))
  }

  val goodValves = valves.values.filter(_.flow > 0).map(_.id)

  @tailrec
  def distance(to: String, distanceSoFar: Int, next: Set[String], visited: Set[String]): Int = {
    if (next.contains(to))
      distanceSoFar
    else {
      distance(to, distanceSoFar + 1, next.map(valves).flatMap(_.next).diff(visited), visited ++ next)
    }
  }

  case class State(at: String, timeLeft: Int, valvesToVisit: Set[String])

  val memo = scala.collection.mutable.Map.empty[State, Int]

  def getPressure(state: State, open: Set[String], pressureSoFar: Int): Int = {
    //println(state, pressureSoFar)
    val pressureFromOpenValves = open.map(valves).map(_.flow).sum * state.timeLeft

    if (memo.contains(state))
      pressureSoFar + pressureFromOpenValves + memo(state)
    else {
      val result = {
        if (state.valvesToVisit.isEmpty)
          pressureSoFar + pressureFromOpenValves
        else {
          val results = state.valvesToVisit.map { next =>
            val distanceTo = distance(next, 0, Set(state.at), Set.empty)
            if (distanceTo > state.timeLeft + 1) {
              pressureSoFar + open.map(valves).map(_.flow).sum * state.timeLeft
            } else {
              val addedPressure = open.map(valves).map(_.flow).sum * (distanceTo + 1)
              getPressure(State(next, state.timeLeft - distanceTo - 1, state.valvesToVisit -- Set(next)), open ++ Set(next), pressureSoFar + addedPressure)
            }
          }
          results.max
        }
      }
      memo.addOne(state -> (result - pressureSoFar - pressureFromOpenValves))
      result
    }
  }

  print("Part 1 computing...")
  val part1Start = System.currentTimeMillis()
  val part1 = getPressure(State("AA", 30, goodValves.toSet), Set.empty, 0)
  println("Part 1 result: " + part1)
  println("Part 1 took " + ((System.currentTimeMillis() - part1Start) / 1000) + "s ")

  def perms(size: Int, values: Set[String]): Set[Set[String]] = {
    if (size == 1)
      values.map(Set(_))
    else {
      val prevPerms = perms(size - 1, values)
      prevPerms.flatMap(perm => values.diff(perm).map(v => Set(v) ++ perm))
    }
  }

  val part2Start = System.currentTimeMillis()
  var i = 0
  val permz = perms(goodValves.size / 2, goodValves.toSet)
  val part2 = permz.map { valvesToVisit =>
    println("Part 2 progress: " + ((0d + i)/permz.size * 100).toInt + "%")
    i = i + 1
    val myBest = getPressure(State("AA", 26, valvesToVisit), Set.empty, 0)
    val elephantBest = getPressure(State("AA", 26, goodValves.toSet.diff(valvesToVisit)), Set.empty, 0)
    myBest + elephantBest
  }.max

  println("Part 2 result: " + part2)
  println("Part 2 took " + ((System.currentTimeMillis() - part2Start) / 1000) + "s ")

}

