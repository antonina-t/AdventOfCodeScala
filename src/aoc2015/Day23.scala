package aoc2015

import scala.io.Source
import scala.util.Using

object Day23 extends App {
  val input = Using(Source.fromFile("input/2015/23.txt")) {
    _.getLines.toArray
  }.get

  val hlf = raw"hlf ([a|b])".r
  val tpl = raw"tpl ([a|b])".r
  val inc = raw"inc ([a|b])".r
  val jmp = raw"jmp ([\+-]?\d+)".r
  val jie = raw"jie ([a|b]), ([\+-]?\d+)".r
  val jio = raw"jio ([a|b]), ([\+-]?\d+)".r

  def runOne(regs: Map[String, Long], pos: Int): (Map[String, Long], Int) = input(pos) match {
    case hlf(r) => (regs + (r -> (regs(r) / 2)), pos + 1)
    case tpl(r) => (regs + (r -> (regs(r) * 3)), pos + 1)
    case inc(r) => (regs + (r -> (regs(r) + 1)), pos + 1)
    case jmp(offset) => (regs, pos + offset.toInt)
    case jie(r, offset) => (regs, if (regs(r) % 2 == 0) pos + offset.toInt else pos + 1)
    case jio(r, offset) => (regs, if (regs(r) == 1) pos + offset.toInt else pos + 1)
  }

  def run(regs: Map[String, Long], pos: Int): Long = {
    if (pos < 0 || pos >= input.length) regs("b")
    else (run _).tupled(runOne(regs, pos))
  }

  val part1 = run(Map("a" -> 0, "b" -> 0), 0)
  println(part1)

  val part2 = run(Map("a" -> 1, "b" -> 0), 0)
  println(part2)
}
