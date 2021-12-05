package aoc2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10 extends App {
  val input = Using(Source.fromFile("input/2016/10.txt")) {
    _.getLines.toSeq
  }.get

  case class Bot(values: Seq[Int] = Seq(), lowTrg: Option[String] = None, highTrg: Option[String] = None)

  val inputPattern = raw"value (\d+) goes to (bot \d+)".r
  val botPattern = raw"(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)".r

  val bots = input.foldLeft(Map[String, Bot]()) {
    case (bots, s) => s match {
      case inputPattern(value, botId) =>
        val bot = bots.getOrElse(botId, Bot())
        bots + (botId -> bot.copy(values = bot.values :+ value.toInt))
      case botPattern(botId, lowTrgId, highTrgId) =>
        val bot = bots.getOrElse(botId, Bot())
        bots + (botId -> bot.copy(lowTrg = Some(lowTrgId), highTrg = Some(highTrgId)))
    }
  }

  @tailrec
  def run(shouldPrint: Bot => Boolean, bots: Map[String, Bot]): Map[String, Bot] =
    bots.find { case (_, bot) => bot.values.length == 2 } match {
      case Some((botId, bot)) =>
        if (shouldPrint(bot))
          println(botId)
        val (lowTrgId, highTrgId) = (bot.lowTrg.get, bot.highTrg.get)
        val lowTrg = bots.getOrElse(lowTrgId, Bot())
        val highTrg = bots.getOrElse(highTrgId, Bot())
        run(shouldPrint, bots +
          (lowTrgId -> lowTrg.copy(values = lowTrg.values :+ bot.values.min)) +
          (highTrgId -> highTrg.copy(values = highTrg.values :+ bot.values.max)) -
          botId)
      case None => bots
    }

  val part1 = run(_.values.sorted == Seq(17, 61), bots)
  val part2 = part1
    .collect {
      case (k, v) if Set("output 0", "output 1", "output 2").contains(k) => v.values.head
    }
    .product
  println(part2)

}
