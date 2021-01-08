package aoc2015

import java.math.BigInteger
import java.security.MessageDigest

object Day4 extends App {
  val input = "yzbqklnj"

  val md = MessageDigest.getInstance("MD5")

  def md5(s: String) = {
    val b = s.getBytes("UTF-8")
    md.update(b, 0, b.length)
    new BigInteger(1, md.digest()).toString(16).reverse.padTo(32, "0").reverse.mkString
  }

  def find(prefix: String) = LazyList.from(1).dropWhile(number => !md5(input + number).startsWith(prefix)).head

  val part1 = find("00000")
  println(part1)

  val part2 = find("000000")
  println(part2)
}
