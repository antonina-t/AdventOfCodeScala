package aoc2021

import scala.io.Source
import scala.util.Using

object Day14 extends App {
  val input = Using(Source.fromFile("input/2021/14.txt")) {
    _.getLines.toSeq
  }.get

  val template = input.head
  val rules = input.drop(2).map(_.split(" -> ")).map { case Array(k, v) => (k, v) }.toMap

  val poly = (1 to 10).foldLeft(template)((p, i) => {
    p.sliding(2).map(pair => rules.get(pair).map(s =>
      Seq(pair(0), s(0), pair(1)).mkString)
      .getOrElse(pair)).flatMap(_.init).mkString + template.last
  })


  val max = poly.groupBy(identity).maxBy(_._2.length)
  val min = poly.groupBy(identity).minBy(_._2.length)

  val part1 = max._2.length - min._2.length
  println(part1)

  val initialCounts = template.sliding(2).toSeq.groupBy(identity).map { case (k, v) => k -> (0L + v.length) }

  val counts = (1 to 40).foldLeft(initialCounts)((counts, _) => {
    val newCounts = collection.mutable.Map() ++ counts
    counts.foreach{case (pair, count) => {
      val start = Seq(pair(0),rules(pair)).mkString
      val end = Seq(rules(pair),pair(1)).mkString
      newCounts(start) = newCounts.getOrElse(start, 0L) + count
      newCounts(end) = newCounts.getOrElse(end, 0L)  + count
      newCounts(pair) = newCounts.getOrElse(pair, 0L)  - count
    }}
    newCounts.toMap
  }) ++ Map("-" + template.head -> 1L, template.last + "-" -> 1L)
  println(counts)

  val allLetters = counts.keys.flatten.toSeq.distinct

  val letterCounts = allLetters.map(l => l -> (counts.filter(_._1.contains(l)).values.sum / 2 + counts.getOrElse(Seq(l,l).mkString, 0L) / 2)).filterNot(_._1 == '-')
  println(letterCounts)
  val max2 = letterCounts.maxBy(_._2)._2
  val min2 = letterCounts.minBy(_._2)._2
  val part2 = max2 - min2
  println(part2)
}
