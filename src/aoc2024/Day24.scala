package aoc2024

import scala.io.Source
import scala.util.Using

object Day24 extends App {

  val input = Using(Source.fromFile("input/2024/24.txt")) {
    _.getLines().toSeq
  }.get

  val in = input.takeWhile(_.nonEmpty).map(_.split(": ")).map {
    case Array(wire, v) => wire -> v.toInt
  }.toMap

  val gates: Map[String, (String, String, String)] = input.drop(in.size + 1).map(_.split(" -> ")).map {
    case Array(op, res) =>
      val split = op.split(" ")
      res -> (split(0), split(2), split(1))
  }.toMap

  val zs = gates.keys.filter(_.startsWith("z")).map(z => (z, z.tail.toInt)).toSeq.sortBy(_._2).map(_._1)

  val swaps = Map(
    "hwk" -> "z06",
    "z06" -> "hwk",
    "tnt" -> "qmd",
    "qmd" -> "tnt",
    "hpc" -> "z31",
    "z31" -> "hpc",
    "z37" -> "cgr",
    "cgr" -> "z37"
  )

  def getValue(wire: String): Int = {
    if (in.contains(wire))
      in(wire)
    else {
      val (a, b, c) = gates(swaps.getOrElse(wire, wire))
      val f: (Int, Int) => Int = c match {
        case "AND" =>
          (a, b) => a & b

        case "OR" =>
          (a, b) => a | b

        case "XOR" =>
          (a, b) => a ^ b
      }
      f(getValue(a), getValue(b))
    }
  }

  val zResults = zs.map(getValue).reverse
  val part1 = zResults.foldLeft(BigInt(0))((acc, z) => {
    acc * 2 + z
  })

  println(part1)

  val xs = in.keys.filter(_.startsWith("x")).map(z => (z, z.tail.toInt)).toSeq.sortBy(_._2).map(_._1)
  val ys = in.keys.filter(_.startsWith("y")).map(z => (z, z.tail.toInt)).toSeq.sortBy(_._2).map(_._1)

  val x = xs.reverse.foldLeft(BigInt(0))((acc, z) => {
    acc * 2 + in(z)
  })

  val y = ys.reverse.foldLeft(BigInt(0))((acc, z) => {
    acc * 2 + in(z)
  })

  def toBinary(n: BigInt, s: String): String = {
    if (n == 0) s
    else
      toBinary(n / 2, n % 2 + s)
  }

  println(toBinary(part1, ""))
  println(toBinary(x + y, ""))

  def replace(v: String): String = {
    if (v.startsWith("x") || v.startsWith("y"))
      v
    else {
      val (a, b, op) = gates(swaps.getOrElse(v, v))
      "(" + replace(a) + " " + op + " " + replace(b) + ")"
    }
  }

  val part2 = swaps.flatMap(pair => Seq(pair._1, pair._2)).toSeq.distinct.sorted.mkString(",")
  println(part2)


}
