package aoc2022

import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("input/2022/13e.txt")) {
    _.getLines().toSeq
  }.get.mkString("\n").split("\n\n").map(_.split("\n"))

  case class Packet(value: Int = -1, contents: Seq[Packet] = Seq.empty) {
    override def toString: String = s"[${if (value != -1) value else contents.mkString(",")}]"
  }

  def toPacket(s: String): Packet = {
    //println(s)
    if (s == "[]")
      Packet()
    else if (!s.contains("[") && !s.contains(","))
      Packet(value = s.toInt)
    else if (!s.tail.init.contains("["))
      Packet(contents = s.tail.init.split(",").map(_.toInt).map(i => Packet(i)))
    else {
      val depths = s.tail.init.scanLeft(0) {
        case (depth, c) =>
          c match {
            case '[' => depth + 1
            case ']' => depth - 1
            case _ => depth
          }
      }
      val sWithDepth = ("-" + s.tail.init).zip(depths).tail
      val splits = splitS(sWithDepth, Seq.empty)
      Packet(contents = splits.map(toPacket))
    }
  }

  def splitS(sWithDepth: Seq[(Char, Int)], result: Seq[Seq[(Char, Int)]]): Seq[String] = {
    if (sWithDepth.isEmpty)
      result.map(_.map(_._1).mkString)
    else {
      val elem = sWithDepth.takeWhile(c => c != (',', 0))
      splitS(sWithDepth.drop(elem.length).dropWhile(_._1 == ','), result :+ elem)
    }
  }


  def compareTo(left: Packet, right: Packet): Int = {
    if (left.value != -1 && right.value != -1)
      left.value - right.value
    else if (left.value == -1 && right.value == -1) {
      if (left.contents.isEmpty && right.contents.isEmpty) 0
      else if (left.contents.isEmpty) -1
      else if (right.contents.isEmpty) 1
      else {
        val comp = compareTo(left.contents.head, right.contents.head)
        if (comp < 0) -1
        else if (comp == 0) compareTo(left.copy(contents = left.contents.tail), right.copy(contents = right.contents.tail))
        else 1
      }
    } else if (left.value == -1) {
      compareTo(left, Packet(-1, Seq(right)))
    } else {
      compareTo(Packet(-1, Seq(left)), right)
    }
  }


  val part1 = input.zipWithIndex.map { case (arr, i) =>
    val left = toPacket(arr(0))
    val right = toPacket(arr(1))
    val comp = compareTo(left, right)
    //println(i, left, right, comp)
    if (comp <= 0) i + 1 else 0
  }.sum
  println(part1)

  val input2 = Using(Source.fromFile("input/2022/13e.txt")) {
    _.getLines().toSeq
  }.get.filter(_.nonEmpty) ++ Seq("[[2]]","[[6]]")


  val part2 = input2.map(toPacket).sortWith((p1, p2) => compareTo(p1, p2) < 0)
  println((part2.indexOf(toPacket("[[2]]")) + 1) * (part2.indexOf(toPacket("[[6]]")) + 1))

}

