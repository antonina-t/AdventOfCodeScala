package aoc2024

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val input = Using(Source.fromFile("input/2024/17.txt")) {
    _.getLines().toSeq
  }.get

  /*
  Register A: 46337277
  Register B: 0
  Register C: 0

  Program: 2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0

  B = A % 8
  B = B XOR 1 // even => +1, odd => -1
  C = A / (2 ^ B) // bit shift B times (0 to 7)
  B = B XOR C
  B = B XOR 4
  A = A / 8 // bit shift 3 times
  out B % 8
  if (A != 0) jump to 0
   */

  def div2(top: BigInt, div: BigInt): BigInt =
    if (top == 0 || div == 0) top else div2(top / 2, div - 1)

  def toOct(n: BigInt, digits: Seq[Int]): String = {
    if (n == BigInt(0)) {
      if (digits.isEmpty)
        "0"
      else digits.mkString
    } else toOct(n / 8, (n % 8).toInt +: digits)
  }

  def compute(A: BigInt, B: BigInt, C: BigInt, pos: Int, program: Seq[Int], out: Seq[Int]): String = {
    if (!program.indices.contains(pos))
      out.mkString(",")
    else {
      val opcode = program(pos)
      opcode match {
        case 0 =>
          val operand = comboOperand(program(pos + 1), A, B, C)
          val result = div2(A, operand)
          compute(result, B, C, pos + 2, program, out)
        case 1 =>
          val operand = program(pos + 1)
          compute(A, B ^ operand, C, pos + 2, program, out)
        case 2 =>
          val operand = comboOperand(program(pos + 1), A, B, C) % 8
          compute(A, operand, C, pos + 2, program, out)
        case 3 =>
          val operand = program(pos + 1)
          compute(A, B, C, if (A != 0) operand else pos + 2, program, out)
        case 4 =>
          compute(A, B ^ C, C, pos + 2, program, out)
        case 5 =>
          val operand = comboOperand(program(pos + 1), A, B, C) % 8
          compute(A, B, C, pos + 2, program, out :+ operand.toInt)
        case 6 =>
          val operand = comboOperand(program(pos + 1), A, B, C)
          val result = div2(A, operand)
          compute(A, result, C, pos + 2, program, out)
        case 7 =>
          val operand = comboOperand(program(pos + 1), A, B, C)
          val result = div2(A, operand)
          compute(A, B, result, pos + 2, program, out)
      }
    }
  }

  def comboOperand(op: BigInt, A: BigInt, B: BigInt, C: BigInt) = op.toInt match {
    case o if o < 4 => BigInt(o)
    case 4 => A
    case 5 => B
    case 6 => C
  }

  val program = "2,4,1,1,7,5,4,4,1,4,0,3,5,5,3,0".split(",").map(_.toInt)
  val part1 = compute(46337277, 0, 0, 0, program, Seq.empty)
  println(part1)

  def getAllB(A: Seq[Int], pos: Int): Seq[Int] = {
    val digits = A.drop(pos * 3).take(3)
    toInts(digits).map(_.toInt)
  }

  def toInts(digits: Seq[Int]) = {
    if (digits.forall(_ == -1))
      (0 to 7).map(BigInt(_))
    else {
      digits.reverse.foldLeft(Seq(BigInt(0)))((acc, digit) => {
        if (digit == -1)
          acc.flatMap(value => Seq(value * 2, value * 2 + 1))
        else
          acc.map(_ * 2 + digit)
      })
    }
  }

  def toDigits(value: BigInt): Seq[Int] = {
    if (value < 8)
      Seq(value.toInt % 2, value.toInt / 2 % 2, value.toInt / 4 % 2)
    else
      (value % 2).toInt +: toDigits(value / 2)
  }

  def findA(A: Seq[Int], pos: Int): Seq[Seq[Int]] = {
    if (pos == program.length)
      Seq(A)
    else {
      val allB = getAllB(A, pos)
      val allNewA = allB.flatMap(B => {
        val newA = A.take(3 * pos) ++ Seq(B % 2, (B / 2) % 2, (B / 4) % 2) ++ A.drop(3 * pos + 3)
        val newB = B ^ 1
        val allC = toInts(newA.drop(pos * 3).slice(newB, newB + newA.drop(pos * 3).drop(newB).lastIndexWhere(_ != -1) + 1))
        val filtered = allC.filter(C =>
          ((newB ^ C) ^ 4) % 8 == program(pos)
        ).map(C => (B, C))
        if (filtered.isEmpty)
          Seq.empty
        else {
          filtered.map {
            case (_, c) => newA.take(pos * 3 + newB) ++ toDigits(c) ++ Seq.fill(100)(-1)
          }
        }
      })
      //println(allNewA.map(a => a.take(a.lastIndexWhere(_ != -1) + 1)).mkString("\n"))
      allNewA.flatMap(a => findA(a, pos + 1))
    }
  }

  val part2 = findA(Seq.fill(100)(-1), 0).map(_.map(i => if (i == -1) 0 else i)).flatMap(toInts).min
  println(part2)

}
