package aoc2015

object Day10 extends App {
  val input = "1321131112"

  val memo = scala.collection.mutable.Map[String, String](
    "1" -> "11",
    "2" -> "12",
    "3" -> "13",
    "11" -> "21",
    "22" -> "22",
    "33" -> "23",
    "111" -> "31",
    "222" -> "32",
    "333" -> "33"
  )

  def split(s: String): (String, String) = {
    val mid = s.length / 2
    if (s(mid - 1) != s(mid))
      (s.take(mid), s.drop(mid))
    else if (s(mid) != s(mid + 1))
      (s.take(mid + 1), s.drop(mid + 1))
    else
      (s.take(mid - 1), s.drop(mid - 1))
  }

  def iter(s: String): String = memo.getOrElseUpdate(s, {
    val (s1, s2) = split(s)
    iter(s1) + iter(s2)
  })

  def lengthAfter(n: Int) = (1 to n).foldLeft(input)((s, _) => iter(s)).length

  val part1 = lengthAfter(40)
  println(part1)

  val part2 = lengthAfter(50)
  println(part2)
}
