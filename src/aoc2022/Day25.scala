package aoc2022

import scala.io.Source
import scala.util.Using

object Day25 extends App {

  val input = Using(Source.fromFile("input/2022/25.txt")) {
    _.getLines().toSeq
  }.get


  val sum = input.map(line => {
    line.reverse.foldLeft(BigInt(0), BigInt(1)) {
      case ((sum, mult), c) => c match {
        case '0' => (sum, mult * 5)
        case '1' => (sum + mult, mult * 5)
        case '2' => (sum + mult * 2, mult * 5)
        case '-' => (sum - mult, mult * 5)
        case '=' => (sum - mult * 2, mult * 5)
      }
    }
  }).map(_._1).sum

  def convert(nbr: String) =
    nbr.reverse.foldLeft(BigInt(0), BigInt(1)) {
      case ((sum, mult), c) => c match {
        case '0' => (sum, mult * 5)
        case '1' => (sum + mult, mult * 5)
        case '2' => (sum + mult * 2, mult * 5)
        case '-' => (sum - mult, mult * 5)
        case '=' => (sum - mult * 2, mult * 5)
      }
    }._1

  def next(nbr: String): String = {
    if (nbr.isEmpty)
      "1"
    else {
      nbr.last match {
        case '=' => nbr.init :+ '-'
        case '-' => nbr.init :+ '0'
        case '0' => nbr.init :+ '1'
        case '1' => nbr.init :+ '2'
        case '2' => next(nbr.init) :+ '='
      }
    }
  }

  def nextR(nbr: String, stop: BigInt): String = {
    println(nbr, convert(nbr))
    if (convert(nbr) == stop)
      nbr
    else nextR(next(nbr), stop)
  }

  //32405707664897
  // 5^19
  println(sum)
  println(sum - convert("2=222-2---2001--000="))

  nextR("2=222-2---2001--000=", sum)

}

