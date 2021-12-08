package aoc2021

import scala.io.Source
import scala.util.Using

object Day8 extends App {
  val input = Using(Source.fromFile("input/2021/8.txt")) {
    _.getLines.toSeq
  }.get.map(_.split("\\|").map(_.trim.split(" ")))

  val part1 = input.map(_ (1).map(_.length).count(l => Set(2, 3, 4, 7).contains(l))).sum
  println(part1)

  def decode(data: Array[String], code: Array[String]) = {
    val length = data.groupBy(_.length)

    val cf = length(2).head //1
    val acf = length(3).head //7
    val bcdf = length(4).head //4
    val abcdefg = length(7).head //8

    val a = acf.diff(cf).head
    val g = (length(5) ++ length(6)).reduce((d1, d2) => d1.intersect(d2)).filter(_ != a).head
    val e = abcdefg.diff(bcdf).diff(acf).filter(_ != g).head
    val bd = bcdf.diff(acf)
    val cde = abcdefg.diff(length(6).reduce((d1, d2) => d1.intersect(d2)))
    val d = bd.intersect(cde).head
    val b = bd.filterNot(_ == d).head
    val c = cde.diff(Seq(d, e)).head
    val f = acf.diff(Seq(a, c)).head

    val bf = Seq(b, f).sorted.mkString
    val be = Seq(b, e).sorted.mkString
    val ce = Seq(c, e).sorted.mkString
    code.map(digit => {
      val notInEight = abcdefg.diff(digit).sorted.mkString
      digit.length match {
        case 2 => 1
        case 3 => 7
        case 4 => 4
        case 7 => 8
        case 5 => Map(bf -> 2, be -> 3, ce -> 5).getOrElse(notInEight, 0)
        case 6 => Map(d -> 0, c -> 6, e -> 9).getOrElse(notInEight.head, 0)
      }
    }).mkString.toInt
  }

  val part2 = input.map {
    case Array(data, code) => decode(data, code)
  }.sum
  println(part2)

}
