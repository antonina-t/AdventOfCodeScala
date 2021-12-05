package aoc2016

import java.math.BigInteger
import java.security.MessageDigest

object Day5 extends App {
  val input = "ffykfhsq"

  val md = MessageDigest.getInstance("MD5")

  def md5(s: String) = {
    val b = s.getBytes("UTF-8")
    md.update(b, 0, b.length)
    new BigInteger(1, md.digest()).toString(16).reverse.padTo(32, "0").reverse.mkString
  }

  val part1 = LazyList
    .from(0)
    .map(number => md5(input + number))
    .filter(_.startsWith("00000"))
    .map(_ (5))
    .take(8)
    .mkString
  println(part1)

  val part2 = "--------".toCharArray
  val _ = LazyList
    .from(0)
    .map(number => md5(input + number))
    .filter(_.startsWith("00000"))
    .takeWhile { hash =>
      val index = hash(5) - '0'
      if (part2.indices.contains(index) && part2(index) == '-') {
        part2(index) = hash(6)
        println(part2.mkString)
      }
      part2.contains('-')
    }
    .mkString

}
