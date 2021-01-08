package aoc2015

import scala.io.Source
import scala.util.Using

object Day7 extends App {
  val input = Using(Source.fromFile("input/2015/7.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = "(.*) -> (.*)".r
  val const = raw"(\d+)".r
  val and = raw"(.*) AND (.*)".r
  val or = raw"(.*) OR (.*)".r
  val lShift = raw"(.*) LSHIFT (\d+)".r
  val rShift = raw"(.*) RSHIFT (\d+)".r
  val not = raw"NOT (.*)".r
  val eq = raw"(.*)".r

  val rules = input.map { case pattern(rule, key) => key -> rule }.toMap

  def getValue(key: String, memo: scala.collection.mutable.Map[String, Int]) = {
    def getValue(key: String): Int = memo.getOrElseUpdate(key, rules.get(key).map {
      case const(a) => a.toInt
      case and(a, b) => getValue(a) & getValue(b)
      case or(a, b) => getValue(a) | getValue(b)
      case lShift(a, b) => (getValue(a) << b.toInt) & 65535
      case rShift(a, b) => getValue(a) >> b.toInt
      case not(a) => ~getValue(a) & 65535
      case eq(a) => getValue(a)
    }.getOrElse(key.toInt))

    getValue(key)
  }

  val part1 = getValue("a", scala.collection.mutable.Map[String, Int]())
  println(part1)

  val part2 = getValue("a", scala.collection.mutable.Map[String, Int]("b" -> part1))
  println(part2)

}
