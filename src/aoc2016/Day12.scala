package aoc2016

import scala.io.Source
import scala.util.Using

object Day12 extends App {
  val input = Using(Source.fromFile("input/2016/12.txt")) {
    _.getLines.toArray
  }.get

  val cpy = "cpy (.*) (.*)".r
  val inc = "inc (.*)".r
  val dec = "dec (.*)".r
  val jnz = "jnz (.*) (.*)".r

  def run(mem: Map[String, Long]) = LazyList.from(0)
    .scanLeft((mem, 0)) {
      case ((mem, pos), _) => input(pos) match {
        case cpy(v, reg) =>
          val value = if (v(0).isLetter) mem(v) else v.toInt
          (mem + (reg -> value), pos + 1)
        case inc(reg) => (mem + (reg -> (mem(reg) + 1)), pos + 1)
        case dec(reg) => (mem + (reg -> (mem(reg) - 1)), pos + 1)
        case jnz(v, step) =>
          val value = if (v(0).isLetter) mem(v) else v.toInt
          (mem, if (value != 0) pos + step.toInt else pos + 1)
      }
    }
    .dropWhile { case (_, pos) => input.indices.contains(pos) }
    .head._1("a")

  val initialState = Map[String, Long](
    "a" -> 0,
    "b" -> 0,
    "c" -> 0,
    "d" -> 0)

  val part1 = run(initialState)
  println(part1)

  val part2 = run(initialState + ("c" -> 1))
  println(part2)
}
