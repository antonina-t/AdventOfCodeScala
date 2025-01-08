package aoc2024

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input = Using(Source.fromFile("input/2024/12.txt")) {
    _.getLines().toSeq
  }.get

  def getRegion(region: (Set[P], Int), next: Set[P]): (Set[P], Int) = {
    if (next.isEmpty)
      region
    else {
      val c = input(next.head.row)(next.head.col)
      val newNext = next.foldLeft((Set.empty[P], 0))((acc, p) => {
        val (nextP, perim) = acc
        val nei = Seq(P(p.row - 1, p.col), P(p.row, p.col + 1), P(p.row + 1, p.col), P(p.row, p.col - 1)).filterNot(region._1.contains)
        val deltaNextP = nei.filter(p => input.indices.contains(p.row) &&
          input.head.indices.contains(p.col) &&
          input(p.row)(p.col) == c
        )
        val deltaPerim = nei.size - deltaNextP.size
        (nextP ++ deltaNextP.toSet, perim + deltaPerim)
      })
      getRegion((region._1 ++ next, region._2 + newNext._2), newNext._1)
    }
  }

  val ps = for (row <- input.indices; col <- input.head.indices) yield P(row, col)
  val regions = ps.foldLeft(Seq.empty[(Set[P], Int)])((regions, p) => {
    if (regions.exists(_._1.contains(p)))
      regions
    else {
      val region = getRegion((Set.empty, 0), Set(p))
      regions :+ region
    }
  })

  val part1 = regions.map(region => region._2 * region._1.size).sum
  println(part1)

  def getSides(region: Set[P]) = {
    val neiHoriUp = region.toSeq.flatMap(p =>
      Set(P(p.row - 1, p.col)).diff(region).toSeq)
    val neiHoriDown = region.toSeq.flatMap(p =>
      Set(P(p.row + 1, p.col)).diff(region).toSeq)
    val neiVertiLeft = region.toSeq.flatMap(p =>
      Set(P(p.row, p.col - 1)).diff(region).toSeq)
    val neiVertiRight = region.toSeq.flatMap(p =>
      Set(P(p.row, p.col + 1)).diff(region).toSeq)

    val hori = neiHoriUp.groupBy(_.row).map {
      case (_, ps) =>
        if (ps.size == 1)
          1
        else {
          val sorted = ps.toSeq.sortBy(_.col)
          val sides = sorted.tail.foldLeft(Seq(Seq(sorted.head)))((acc, p) => {
            if (p.col - 1 == acc.last.last.col)
              acc.init :+ (acc.last :+ p)
            else
              acc :+ Seq(p)
          })
          sides.size
      }
    }.sum + neiHoriDown.groupBy(_.row).map {
      case (_, ps) =>
        if (ps.size == 1)
          1
        else {
          val sorted = ps.toSeq.sortBy(_.col)
          val sides = sorted.tail.foldLeft(Seq(Seq(sorted.head)))((acc, p) => {
            if (p.col - 1 == acc.last.last.col)
              acc.init :+ (acc.last :+ p)
            else
              acc :+ Seq(p)
          })
          sides.size
        }
    }.sum

    val verti = neiVertiLeft.groupBy(_.col).map {
      case (_, ps) =>
        if (ps.size == 1)
          1
        else {
          val sorted = ps.toSeq.sortBy(_.row)
          val sides = sorted.tail.foldLeft(Seq(Seq(sorted.head)))((acc, p) => {
            if (p.row - 1 == acc.last.last.row)
              acc.init :+ (acc.last :+ p)
            else
              acc :+ Seq(p)
          })
          sides.size
        }
    }.sum + neiVertiRight.groupBy(_.col).map {
      case (_, ps) =>
        if (ps.size == 1)
          1
        else {
          val sorted = ps.toSeq.sortBy(_.row)
          val sides = sorted.tail.foldLeft(Seq(Seq(sorted.head)))((acc, p) => {
            if (p.row - 1 == acc.last.last.row)
              acc.init :+ (acc.last :+ p)
            else
              acc :+ Seq(p)
          })
          sides.size
        }
    }.sum

    hori + verti
  }


  val part2 = regions.map(region => {
    val sides = getSides(region._1)
    println(s"${input(region._1.head.row)(region._1.head.col)} = ${region._1.size} * $sides")
    sides * region._1.size
  }).sum

  println(part2)

}

case class P(row: Int, col: Int) {
  def c(input: Seq[String]): Char = input(row)(col)

}
