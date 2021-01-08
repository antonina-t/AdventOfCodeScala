package aoc2015

import scala.io.Source
import scala.util.Using

object Day13 extends App {
  val input = Using(Source.fromFile("input/2015/13.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = "(.*) would (gain|lose) (.*) happiness units by sitting next to (.*).".r

  val happiness = input.map {
    case pattern(src, gain, value, trg) =>
      s"$src-$trg" -> value.toInt * (if (gain == "gain") 1 else -1)
  }.toMap

  val people = input.flatMap {
    case pattern(src, _, _, trg) => Seq(src, trg)
  }.distinct

  def permutations(people: Seq[String]): Seq[Seq[String]] = people match {
    case Seq() => Seq()
    case Seq(a) => Seq(Seq(a))
    case _ => people.flatMap(p => permutations(people.filter(_ != p)).map(p +: _))
  }

  def totalHappiness(people: Seq[String]) =
    people
      .zip(people.tail :+ people.head)
      .map { case (a, b) => happiness.getOrElse(s"$a-$b", 0) + happiness.getOrElse(s"$b-$a", 0) }
      .sum

  val part1 = permutations(people).map(totalHappiness).max
  println(part1)

  val part2 = permutations(people :+ "you").map(totalHappiness).max
  println(part2)
}
