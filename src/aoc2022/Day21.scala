package aoc2022

import scala.io.Source
import scala.util.Using

object Day21 extends App {

  val input = Using(Source.fromFile("input/2022/21.txt")) {
    _.getLines().toSeq
  }.get

  val monkeys = input.map(line => {
    val splitAt = line.indexOf(" ")
    line.substring(0, splitAt - 1) -> line.substring(splitAt + 1)
  }).toMap

  val nbrPattern = raw"(\d+)".r
  val plusPattern = raw"(\w+) \+ (\w+)".r
  val minusPattern = raw"(\w+) \- (\w+)".r
  val timesPattern = raw"(\w+) \* (\w+)".r
  val divPattern = raw"(\w+) / (\w+)".r
  def getMonkeyNumber(monkey: String, visited: scala.collection.mutable.Map[String, Long]): Long = {
    //println(monkey)
    visited.getOrElseUpdate(monkey, {
      val command = monkeys(monkey)
      command match {
        case nbrPattern(nbr) => nbr.toLong
        case plusPattern(a, b) => getMonkeyNumber(a, visited) + getMonkeyNumber(b, visited)
        case minusPattern(a, b) => getMonkeyNumber(a, visited) - getMonkeyNumber(b, visited)
        case timesPattern(a, b) => getMonkeyNumber(a, visited) * getMonkeyNumber(b, visited)
        case divPattern(a, b) => getMonkeyNumber(a, visited) / getMonkeyNumber(b, visited)
      }
    })

  }

  val part1 = getMonkeyNumber("root", scala.collection.mutable.Map.empty[String, Long])
  println(part1)

  def contains(expr: String, monkey: String): Boolean = {
    if (expr == monkey) true
    else {
      val command = monkeys(expr)
      if (!command.contains(" "))
        false
      else {
        val split = command.split(" ")
        contains(split(0), monkey) || contains(split(2), monkey)
      }
    }
  }

  val left = monkeys("root").split(" ")(0)
  val right = monkeys("root").split(" ")(2)
  val leftContains = contains(left, "humn")
  val rightContains = contains(right, "humn")

  val part2 = if (leftContains && !rightContains) {
    val rightValue = getMonkeyNumber(right, scala.collection.mutable.Map.empty[String, Long])
    expand(left, rightValue)
  } else if (!leftContains && rightContains) {
    val leftValue = getMonkeyNumber(left, scala.collection.mutable.Map.empty[String, Long])
    expand(right, leftValue)
  } else {
    throw new Exception("Both left and right values of root contain humn")
  }

  println(part2)
  def expand(expr: String, value: Long): Long = {
    //println(expr, value)
    if (expr == "humn")
      value
    else {
      val command = monkeys(expr)
      command match {
        case nbrPattern(nbr) => throw new Exception(nbr + " should be equal to " + value + "?")
        case _ =>
          val split = command.split(" ")
          val leftContains = contains(split(0), "humn")
          val rightContains = contains(split(2), "humn")
          if (leftContains && !rightContains) {
            val rightValue = getMonkeyNumber(split(2), scala.collection.mutable.Map.empty[String, Long])
            split(1) match {
              case "+" => expand(split(0), value - rightValue)
              case "-" => expand(split(0), value + rightValue)
              case "*" => expand(split(0), value / rightValue)
              case "/" => expand(split(0), value * rightValue)
            }
          } else if (!leftContains && rightContains) {
            val leftValue = getMonkeyNumber(split(0), scala.collection.mutable.Map.empty[String, Long])
            split(1) match {
              case "+" => expand(split(2), value - leftValue)
              case "-" => expand(split(2), leftValue - value)
              case "*" => expand(split(2), value / leftValue)
              case "/" => expand(split(2), leftValue / value)
            }
          } else {
            throw new Exception(expr + " " + value + " both contain humn")
          }
      }
    }
  }

  //expand("jwcq", getMonkeyNumber("swbn", scala.collection.mutable.Map.empty[String, Long]))
}

