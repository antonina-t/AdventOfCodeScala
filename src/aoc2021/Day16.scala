package aoc2021

import scala.io.Source
import scala.util.Using

object Day16 extends App {
  val input = Using(Source.fromFile("input/2021/16.txt")) {
    _.getLines.toSeq
  }.get.head

  val binaryString = BigInt(input, 16).toString(2)
  val binaryStringPadded = input.takeWhile(_ == '0').map(_ => "0000").mkString ++ (binaryString.length % 4 match {
    case 0 => binaryString
    case 1 => "000" + binaryString
    case 2 => "00" + binaryString
    case 3 => "0" + binaryString
  })

  case class Packet(ver: Int, num: Int, data: BigInt, packets: Seq[Packet])

  @scala.annotation.tailrec
  def parseMany(packets: Seq[Packet], s: String): (Seq[Packet], String) = {
    if (!s.contains("1")) (packets, s)
    else {
      val (p, rest) = parseOne(s)
      parseMany(packets :+ p, rest)
    }
  }

  def parseOne(s: String): (Packet, String) = {
    val ver = Integer.parseInt(s.take(3), 2)
    val num = Integer.parseInt(s.slice(3, 6), 2)
    if (num == 4) {
      val bits = s.drop(6).sliding(5,5).toSeq
      val init = bits.takeWhile(_.startsWith("1")).mkString
      val last = bits.dropWhile(_.startsWith("1")).head
      val length = (init + last).length
      val data = (bits.takeWhile(_.startsWith("1")).map(_.tail) :+ bits.dropWhile(_.startsWith("1")).head.tail).flatten.mkString
      (Packet(ver, num, BigInt(data, 2), Seq.empty), s.drop(6 + length))
    } else {
      val lenType = s.slice(6, 7)
      if (lenType == "0") {
        val packetsLength = Integer.parseInt(s.slice(7, 22), 2)
        val (packets, _) = parseMany(Seq.empty, s.slice(22, 22 + packetsLength))
        (Packet(ver, num, -1, packets), s.drop(22 + packetsLength))
      } else if (lenType == "1") {
        val packetsCount = Integer.parseInt(s.slice(7, 18), 2)
        val (packets, rest) = (1 to packetsCount).foldLeft((Seq.empty[Packet], s.drop(18)))((acc, _) => {
          val (packets, rest) = acc
          val (packet, newRest) = parseOne(rest)
          (packets :+ packet, newRest)
        })
        (Packet(ver, num, -1, packets), rest)
      } else throw new RuntimeException("Wrong lenType")
    }
  }

  def getVerNumSum(p: Packet): Int = p.ver + p.packets.map(getVerNumSum).sum

  val packet = parseOne(binaryStringPadded)._1

  val part1 = getVerNumSum(packet)
  println(part1)

  def calculate(packet: Packet): BigInt = {
    val calculated = packet.packets.map(calculate)
    packet.num match {
      case 0 => calculated.sum
      case 1 => calculated.product
      case 2 => calculated.min
      case 3 => calculated.max
      case 4 => packet.data
      case 5 => if (calculated.head > calculated.last) 1 else 0
      case 6 => if (calculated.head < calculated.last) 1 else 0
      case 7 => if (calculated.head == calculated.last) 1 else 0
    }
  }

  val part2 = calculate(packet)
  println(part2)

}
