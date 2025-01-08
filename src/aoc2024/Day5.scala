package aoc2024

import scala.io.Source
import scala.util.Using

object Day5 extends App {

  val input = Using(Source.fromFile("input/2024/5.txt")) {
    _.getLines().toSeq
  }.get

  val rules = input.takeWhile(_.nonEmpty).map(_.split("\\|").map(_.toInt))
  val updates = input.drop(rules.length + 1).map(_.split(",").map(_.toInt))

  def isCorrect(update: Array[Int]) = {
    rules.forall(rule => {
      !update.contains(rule(0)) || !update.contains(rule(1)) || update.indexOf(rule(0)) < update.indexOf(rule(1))
    })
  }

  val part1 = updates.filter(isCorrect).map(update => update(update.length / 2)).sum
  println(part1)

  def abideRule(update: Array[Int], rule: Array[Int]): Array[Int] = {
    val indexLeft = update.indexOf(rule(0))
    val indexRight = update.indexOf(rule(1))
    if (indexLeft > indexRight) {
      val tmp = update(indexLeft)
      update(indexLeft) = update(indexRight)
      update(indexRight) = tmp
    }
    update
  }

  def abideAll(update: Array[Int], rules: Seq[Array[Int]]): Array[Int] = {
    if (rules.forall(rule => update.indexOf(rule(0)) < update.indexOf(rule(1))))
      update
    else {
      rules.foreach(rule => abideRule(update, rule))
      abideAll(update, rules)
    }
  }

  val part2 = updates.filterNot(isCorrect).map(update => {
    val myRules = rules.filter(rule => rule.forall(update.contains))
    abideAll(update, myRules)(update.length / 2)
  }).sum

  println(part2)

}
