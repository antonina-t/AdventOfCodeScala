package aoc2024

import scala.io.Source
import scala.util.Using

object Day7 extends App {

  val input = Using(Source.fromFile("input/2024/7.txt")) {
    _.getLines().toSeq
  }.get

  val pattern = raw"(\d+): (.*)".r

  def op1(n1: Long, n2: Long): Seq[Long] =
    Seq(n1 + n2, n1 * n2)

  def test(testValue: Long, result: Long, values: Seq[Long], op: (Long, Long) => Seq[Long]): Boolean = {
    if (values.isEmpty) testValue == result
    else
      op(result, values.head).exists(result =>
        test(testValue, result, values.tail, op)
      )
  }

  def getResult(op: (Long, Long) => Seq[Long]) = input.map {
    case pattern(testValue, valuesStr) =>
      val values = valuesStr.split(" ").map(_.toLong)
      if (test(testValue.toLong, values.head, values.tail, op)) testValue.toLong else 0
  }.sum

  val part1 = getResult(op1)
  println(part1)

  def op2(n1: Long, n2: Long): Seq[Long] =
    op1(n1, n2) :+ (n1.toString ++ n2.toString).toLong

  val part2 = getResult(op2)
  println(part2)

}
