package aoc2023

import scala.io.Source
import scala.util.Using

object Day7 extends App {

  val input = Using(Source.fromFile("input/2023/7.txt")) {
    _.getLines().toSeq
  }.get

  val cards = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse
  val cardsWithJs = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse

  val hands = input.map(_.split(" ")).map { case Array(hand, bid) => hand -> bid.toInt }.toMap

  def getType(hand: String): Int = {
    val combo = hand.groupBy(identity).map(_._2.length).toSeq.sorted.reverse
    Seq(Seq(1, 1, 1, 1, 1), Seq(2, 1, 1, 1), Seq(2, 2, 1), Seq(3, 1, 1), Seq(3, 2), Seq(4, 1), Seq(5)).indexOf(combo)
  }

  def getTypeWithJs(hand: String): Int = {
    val comboWoJs = hand.filterNot(_ == 'J').groupBy(identity).map(_._2.length).toSeq.sorted.reverse
    val combo = if (comboWoJs.isEmpty) Seq(5) else (comboWoJs.head + hand.count(_ == 'J')) +: comboWoJs.tail
    Seq(Seq(1, 1, 1, 1, 1), Seq(2, 1, 1, 1), Seq(2, 2, 1), Seq(3, 1, 1), Seq(3, 2), Seq(4, 1), Seq(5)).indexOf(combo)
  }

  def getTotalWinnings(withJs: Boolean) = hands.keySet.toSeq.sortWith((s1, s2) => {
    val type1 = if (withJs) getTypeWithJs(s1) else getType(s1)
    val type2 = if (withJs) getTypeWithJs(s2) else getType(s2)
    if (type1 != type2) type1 < type2 else {
      val pair = s1.zip(s2).dropWhile { case (c1, c2) => c1 == c2 }.head
      val order = if (withJs) cardsWithJs else cards
      order.indexOf(pair._1) < order.indexOf(pair._2)
    }
  }).foldLeft((1, 0)) {
    case ((rank, totalWinnings), hand) => (rank + 1, totalWinnings + rank * hands(hand))
  }._2

  val part1 = getTotalWinnings(false)
  println(part1)

  val part2 = getTotalWinnings(true)
  println(part2)


}

