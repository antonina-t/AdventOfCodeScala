package aoc2021

import scala.io.Source
import scala.util.Using

object Day24 extends App {
  val input = Using(Source.fromFile("input/2021/24.txt")) {
    _.getLines.toSeq
  }.get

  val inp = raw"inp (.*)".r
  val add = raw"add (.*) (.*)".r
  val mul = raw"mul (.*) (.*)".r
  val div = raw"div (.*) (.*)".r
  val mod = raw"mod (.*) (.*)".r
  val eql = raw"eql (.*) (.*)".r

  def inst(vars: Map[String, Int], s: String, input: Seq[Int]): (Map[String, Int], Seq[Int]) =
    s match {
      case inp(a) => (vars + (a -> input.head), input.tail)
      case add(a, b) => if ("xyzw".contains(b)) (vars + (a -> (vars(a) + vars(b))), input) else (vars + (a -> (vars(a) + b.toInt)), input)
      case mul(a, b) => if ("xyzw".contains(b)) (vars + (a -> (vars(a) * vars(b))), input) else (vars + (a -> (vars(a) * b.toInt)), input)
      case div(a, b) => if ("xyzw".contains(b)) (vars + (a -> (vars(a) / vars(b))), input) else (vars + (a -> (vars(a) / b.toInt)), input)
      case mod(a, b) => if ("xyzw".contains(b)) (vars + (a -> (vars(a) % vars(b))), input) else (vars + (a -> (vars(a) % b.toInt)), input)
      case eql(a, b) => if ("xyzw".contains(b)) (vars + (a -> (if (vars(a) == vars(b)) 1 else 0)), input) else (vars + (a -> (if (vars(a) == b.toInt) 1 else 0)), input)
    }

  def test(number: Long): Boolean = {
    input.foldLeft((Map("x" -> 0, "y" -> 0, "z" -> 0, "w" -> 0), number.toString.chars.map(_ - '0').toArray.toSeq))((acc, s) => {
      val (vars, numbers) = acc
      inst(vars, s, numbers)
    })._1("z") == 0
  }

  /*
  Solved by hand...
  i4 = i3 - 7
  i6 = i5
  i8 = i7 + 5
  i9 = i2 + 3
  i10 = i1 - 5
  i11 = i0 + 4
  i3 = i12 - 1
   */

  val part1 = 59692994994998L
  println(part1)
  println(test(part1))

  val part2 = 16181111641521L
  println(part2)
  println(test(part2))

}
