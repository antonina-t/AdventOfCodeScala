package aoc2016

import java.math.BigInteger
import java.security.MessageDigest

object Day14 extends App {
  val input = "yjdafjpo"

  val md = MessageDigest.getInstance("MD5")

  def md5(s: String) = {
    val b = s.getBytes("UTF-8")
    md.update(b, 0, b.length)
    new BigInteger(1, md.digest()).toString(16).reverse.padTo(32, "0").reverse.mkString
  }

  def get64thKey(stretch: Boolean) = {
    val hashes = scala.collection.mutable.Map[Int, String]()

    def hash(i: Int) = hashes.getOrElseUpdate(i,
      if (stretch) (1 to 2016).foldLeft(md5(input + i))((hash, _) => md5(hash))
      else md5(input + i)
    )

    LazyList.from(0).scanLeft((-1, false)) {
      case ((_, _), i) =>
        val isKey = hash(i).sliding(3).find(_.distinct.length == 1).exists(s => {
          val digit = s(0)
          ((i + 1) to (i + 1000)).exists(i => hash(i)
            .sliding(5)
            .exists(s => s.distinct.length == 1 && s(0) == digit)
          )
        })
        (i, isKey)
    }.filter(_._2).drop(63).head._1
  }

  val part1 = get64thKey(false)
  println(part1)

  val part2 = get64thKey(true)
  println(part2)

}
