package aoc2015

import scala.annotation.tailrec

object Day11 extends App {
  val input = "cqjxjnds"

  def increment(pass: String): String = {
    if (pass.isEmpty)
      "a"
    else if (pass.last != 'z')
      pass.init + (pass.last + 1).toChar
    else
      increment(pass.init) + 'a'
  }

  val conditions = Seq[String => Boolean](
    s => s.sliding(3).exists(s => s(0) == s(1) - 1 && s(1) == s(2) - 1),
    s => !s.exists(c => "iol".contains(c)),
    s => ('a' to 'z').zip('a' to 'z')
      .map { case (a, b) => "" + a + b }
      .count(pair => s.contains(pair)) >= 2
  )

  @tailrec
  def findNext(pass: String): String =
    if (conditions.forall(_ (pass))) pass else findNext(increment(pass))

  val part1 = findNext(input)
  println(part1)

  val part2 = findNext(increment(part1))
  println(part2)
}
