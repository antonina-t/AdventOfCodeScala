package aoc2023

import scala.io.Source
import scala.util.Using

object Day8 extends App {

  val input = Using(Source.fromFile("input/2023/8.txt")) {
    _.getLines().toSeq
  }.get

  val lr = input.head

  val pattern = raw"(\w*) = \((\w*), (\w*)\)".r

  val nodesLR = input.drop(2).map {
    case pattern(node, left, right) => node -> Map('L' -> left, 'R' -> right)
  }.toMap

  @scala.annotation.tailrec
  def go(length: Int, node: String, lrPos: Int): Int = {
    if (node == "ZZZ") length
    else go(length + 1, nodesLR(node)(lr(lrPos)), (lrPos + 1) % lr.length)
  }

  val part1 = go(0, "AAA", 0)
  println(part1)

  val startNodes = nodesLR.keySet.filter(_.endsWith("A")).toSeq

  def getOffsetsAndPeriods(length: Int, nodes: Seq[(String, Int)], distinctNodes: Set[(String, Int)]): (Int, Int) = {
    if (distinctNodes.size < nodes.length) {
      val period = nodes.length - 1 - nodes.indexOf(nodes.last)
      val result = (nodes.indexWhere(_._1.endsWith("Z")), period)
      println(result)
      result
    }
    else {
      val newNode = (nodesLR(nodes.last._1)(lr(length % lr.length)), (length + 1) % lr.length)
      getOffsetsAndPeriods(length + 1, nodes :+ newNode, distinctNodes ++ Set(newNode))
    }
  }

  val offsetsAndPeriods = startNodes.map(startNode => getOffsetsAndPeriods(0, Seq((startNode, 0)), Set((startNode, 0))))

  def gcd(a: BigInt, b: BigInt):BigInt=if (b==BigInt(0)) a.abs else gcd(b, a%b)
  def lcm(a: BigInt, b: BigInt): BigInt=(a*b).abs/gcd(a,b)

  def find(mult: BigInt, offset: BigInt, period: BigInt, values: Seq[(Int, Int)]): BigInt = {
    val length = offset + mult * period
    val matching = values.filter {
      case (offset, period) => (length - offset) % period == 0
    }

    if (values.isEmpty)
      length
    else if (matching.isEmpty)
      find(mult + 1, offset, period, values)
    else {
      find(0, length, lcm(period.toInt, matching.head._2), values.filterNot(matching.contains))
    }

  }

  val part2 = find(0, offsetsAndPeriods.head._1, offsetsAndPeriods.head._2, offsetsAndPeriods.tail)

  println(part2)
}

