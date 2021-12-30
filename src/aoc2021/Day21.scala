package aoc2021

object Day21 extends App {
  val p1Pos = 6;
  val p2Pos = 1

  // Example:
  //val p1Pos = 4; val p2Pos = 8

  def play(p1: Int, p2: Int, p1Sum: Int, p2Sum: Int, die: Int, dieThrown: Int): Int = {
    //println(p1, p2, p1Sum, p2Sum, die, dieThrown)
    if (p1Sum >= 1000) p2Sum * dieThrown
    else if (p2Sum >= 1000) p1Sum * dieThrown
    else {
      val roll = Seq(die, die + 1, die + 2).map(d => if (d > 100) d - 100 else d).sum
      val newP1Raw = p1 + (roll % 10)
      val newP1 = if (newP1Raw > 10) newP1Raw - 10 else newP1Raw
      val newP1Sum = p1Sum + newP1
      if (newP1Sum >= 1000)
        play(newP1, p2, newP1Sum, p2Sum, if (die + 3 > 100) die + 3 - 100 else die + 3, dieThrown + 3)
      else {
        val roll = Seq(die + 3, die + 4, die + 5).map(d => if (d > 100) d - 100 else d).sum
        val newP2Raw = p2 + (roll % 10)
        val newP2 = if (newP2Raw > 10) newP2Raw - 10 else newP2Raw
        val newP2Sum = p2Sum + newP2
        play(newP1, newP2, newP1Sum, newP2Sum, if (die + 6 > 100) die + 6 - 100 else die + 6, dieThrown + 6)
      }
    }
  }

  val part1 = play(p1Pos, p2Pos, 0, 0, 1, 0)
  println(part1)

  def move(p: Int, step: Int) = if (p + step > 10) p + step - 10 else p + step

  val unis = (for (x <- 1 to 3; y <- 1 to 3; z <- 1 to 3) yield (x + y + z)).groupBy(identity).map { case (x, xs) => x -> xs.length }

  @scala.annotation.tailrec
  def play(p: Seq[Map[(Int, Int), Long]]): Seq[Map[(Int, Int), Long]] = {
    //println(p)
    if (p.last.forall(_._1._2 >= 21))
      p
    else {
      val curr = p.last
      val maps = curr
        .filter(_._1._2 < 21)
        .map { case ((pos, score), universes) =>
          unis.map { case (roll, count) =>
            ((move(pos, roll), score + move(pos, roll)), universes * count)
          }
        }.toSeq
      val reduced = maps.reduce((m1, m2) => {
        val res = m1.keySet.union(m2.keySet).map(k => (k, m1.getOrElse(k, 0L) + m2.getOrElse(k, 0L))).toMap
        res
      })
      play(p :+ reduced)
    }
  }

  val p1Play = play(Seq(Map((p1Pos, 0) -> 1L)))
  val p2Play = play(Seq(Map((p2Pos, 0) -> 1L)))

  val p1Wins = p1Play.map(_.filter(_._1._2 >= 21))
  val p1WinCount = p1Wins.indices.filter(p1Wins(_).nonEmpty).map(i1 => {
    val i2 = i1 - 1
    (for (p1 <- p1Wins(i1).values; p2 <- p2Play(i2).filter(_._1._2 < 21).values) yield p1 * p2).sum
  }).sum

  val p2Wins = p2Play.map(_.filter(_._1._2 >= 21))
  val p2WinCount = p2Wins.indices.filter(p2Wins(_).nonEmpty).map(i2 => {
    val i1 = i2
    (for (p2 <- p2Wins(i2).values; p1 <- p1Play(i1).filter(_._1._2 < 21).values) yield p1 * p2).sum
  }).sum

  println(Math.max(p1WinCount, p2WinCount))
}
