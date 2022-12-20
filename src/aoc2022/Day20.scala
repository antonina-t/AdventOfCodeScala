package aoc2022

import scala.io.Source
import scala.util.Using

object Day20 extends App {

  val input = Using(Source.fromFile("input/2022/20.txt")) {
    _.getLines().toSeq
  }.get

  case class Number(value: Long, index: Int)

  val arr = input.zipWithIndex.map { case (n, i) => Number(n.toInt, i) }.toList

  def getTo(from: Int, steps: Long): Int = {
    if (steps == 0) from
    else {
      val cappedSteps = (steps % (input.length - 1)).toInt
      if (cappedSteps > 0) {
        if (from + cappedSteps >= input.length - 1)
          (from + cappedSteps + 1) % input.length
        else
          from + cappedSteps
      } else {
        if (from + cappedSteps <= 0)
          from + cappedSteps + input.length - 1
        else
          from + cappedSteps
      }
    }
  }

  def mix(input: List[Number]) = {
    input.indices.foldLeft(input) {
      case (acc, i) =>
        val from = acc.indexWhere(_.index == i)
        val to = getTo(from, acc(from).value)
        if (from == to) {
          acc
        } else if (from < to) {
          acc.take(from) ++ acc.slice(from + 1, to + 1) ++ Seq(acc(from)) ++ acc.drop(to + 1)
        } else {
          acc.take(to) ++ Seq(acc(from)) ++ acc.slice(to, from) ++ acc.drop(from + 1)
        }
    }
  }

  val arrAfterMix = mix(arr)

  def calculateResult(arr: List[Number]) = {
    val indexOfZero = arr.indexWhere(_.value == 0)
    val one = arr((indexOfZero + 1000) % input.length).value
    val two = arr((indexOfZero + 2000) % input.length).value
    val three = arr((indexOfZero + 3000) % input.length).value

    one + two + three
  }

  println(calculateResult(arrAfterMix))

  val arr2 = arr.map(n => n.copy(value = n.value * 811589153))

  def mixNTimes(n: Int, input: List[Number]): List[Number] =
    if (n == 0) input else mixNTimes(n - 1, mix(input))

  val arrAfterMix2 = mixNTimes(10, arr2)

  println(calculateResult(arrAfterMix2))

}

