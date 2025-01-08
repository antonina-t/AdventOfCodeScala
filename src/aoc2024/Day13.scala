package aoc2024

import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("input/2024/13.txt")) {
    _.getLines().toSeq
  }.get

  /*
  Button A: X+45, Y+76
  Button B: X+84, Y+14
  Prize: X=9612, Y=4342
   */

  val patternA = raw"Button A\: X\+(\d+), Y\+(\d+)".r
  val patternB = raw"Button B\: X\+(\d+), Y\+(\d+)".r
  val prizePattern = raw"Prize\: X=(\d+), Y=(\d+)".r

  def result = input.sliding(4, 4).map(machine => {
    val (a1, a2) = machine(0) match {
      case patternA(a1, a2) => (BigInt(a1), BigInt(a2))
    }
    val (b1, b2) = machine(1) match {
      case patternB(b1, b2) => (BigInt(b1), BigInt(b2))
    }
    val (x, y) = machine(2) match {
      case prizePattern(x, y) => (delta + BigInt(x), delta + BigInt(y))
    }

    val topB = a1 * y - a2 * x
    val bottomB = a1 * b2 - a2 * b1
    if (topB % bottomB == 0) {
      val b = topB / bottomB
      val topA = (x - b1 * b)
      val bottomA = a1
      if (topA % bottomA == 0) {
        val a = (x - b1 * b) / a1
        a * 3 + b
      } else {
        BigInt("0")
      }
    } else BigInt("0")
  }).toSeq.sum

  var delta = BigInt("0")
  println(result)
  delta = BigInt("10000000000000")
  println(result)

}
