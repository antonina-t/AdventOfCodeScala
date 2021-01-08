package aoc2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12 extends App {
  val input = Using(Source.fromFile("input/2015/12.txt")) {
    _.mkString
  }.get

  def sum(s: String) = raw"-?\d+".r.findAllIn(s).map(_.toInt).sum

  val part1 = sum(input)
  println(part1)

  def removeFirstRed(s: String): String = {
    val index = s.indexOf(":\"red\"")
    if (index == -1) s
    else {
      val start = index - (0 to index).reverse.scanLeft(0)((acc, i) => s(i) match {
        case '}' => acc + 1
        case '{' => acc - 1
        case _ => acc
      }).tail.indexOf(-1)
      val end = index + (index until s.length).scanLeft(0)((acc, i) => s(i) match {
        case '{' => acc + 1
        case '}' => acc - 1
        case _ => acc
      }).indexOf(-1)
      s.take(start) + '0' + s.drop(end)
    }
  }

  @tailrec
  def removeAllRed(s: String): String = {
    val result = removeFirstRed(s)
    if (result == s) result else removeAllRed(result)
  }

  val part2 = sum(removeAllRed(input))
  println(part2)
}
