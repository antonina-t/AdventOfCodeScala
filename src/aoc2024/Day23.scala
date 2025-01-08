package aoc2024

import scala.io.Source
import scala.util.Using

object Day23 extends App {

  val input = Using(Source.fromFile("input/2024/23.txt")) {
    _.getLines().toSeq
  }.get

  val nei = collection.mutable.Map.empty[String, Set[String]]
  input.foreach(s => {
    val split = s.split("-")
    val c1 = split(0)
    val c2 = split(1)
    if (!nei.contains(c1))
      nei(c1) = Set.empty
    nei(c1) = nei(c1) + c2
    if (!nei.contains(c2))
      nei(c2) = Set.empty
    nei(c2) = nei(c2) + c1
  })

  def allPairs(s: Set[String]): Set[(String, String)] = {
    if (s.size < 2) Set.empty
    else {
      s.tail.map(t => (s.head, t)) ++ allPairs(s.tail)
    }
  }

  val ts = nei.keys.filter(_.startsWith("t")).flatMap(t => {
    val neis = nei(t)
    allPairs(neis).filter(pair => nei(pair._1).contains(pair._2)).map(pair => Seq(t, pair._1, pair._2).sorted)
  }).filter(_.nonEmpty).toSeq.distinct

  println(ts.size)

  def largestClique(cliques: Set[Set[String]]): Set[Set[String]] = {
    val qs = cliques.flatMap(t => {
      nei.keys.toSet.diff(t).filter(n => t.forall(nei(n).contains)).map(n => t + n)
    }).filter(_.nonEmpty)

    if (qs.isEmpty) cliques else largestClique(qs)
  }

  val q3 = nei.keys.flatMap(t => {
    val neis = nei(t)
    allPairs(neis).filter(pair => nei(pair._1).contains(pair._2)).map(pair => Set(t, pair._1, pair._2))
  }).filter(_.nonEmpty).toSet

  println(largestClique(q3).head.toSeq.sorted.mkString(","))

}
