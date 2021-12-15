package aoc2021

import scala.io.Source
import scala.util.Using

object Day10 extends App {
  val input = Using(Source.fromFile("input/2021/10.txt")) {
    _.getLines.toSeq
  }.get

  val mirror = Map(')'->'(','}'->'{',']'->'[','>'->'<','('->')','{'->'}','['->']','<'->'>')

  val part1 = input.map(line => line.foldLeft(Seq[Char]())((q, c) =>
    if (q.nonEmpty && q.head == '-') q
    else if ("({[<".contains(c)) c +: q
    else if (q.head == mirror(c)) q.tail
    else Seq('-', c))
  )
    .filter(_.headOption.contains('-')).map(_(1)).map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum
  println(part1)

  val part2 = input.map(line => line.foldLeft(Seq[Char]())((q, c) =>
    if (q.nonEmpty && q.head == '-') q
    else if ("({[<".contains(c)) c +: q
    else if (q.head == mirror(c)) q.tail
    else Seq('-', c))
  )
    .filter(line => line.nonEmpty && !line.startsWith("-")).map(_.map(c => mirror(c))
    .foldLeft(0L)((sum, c) => sum * 5 + (c match {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    }))).sorted
  println(part2(part2.length / 2))

}
