package aoc2021

import scala.io.Source
import scala.util.Using

object Day20 extends App {
  val input = Using(Source.fromFile("input/2021/20.txt")) {
    _.getLines.toSeq
  }.get

  val algo = input.takeWhile(_.nonEmpty).mkString
  val image = input.dropWhile(_.nonEmpty).drop(1)

  val pixels = image.indices.flatMap(y => image(y).indices.map(x => ((x, y), image(y)(x)))).filter(_._2 == '#').map(_._1).toSet

  def run(pixels: Set[(Int, Int)], isBorderOn: Boolean): Set[(Int, Int)] = {
    val minX = pixels.map(_._1).min - 1
    val maxX = pixels.map(_._1).max + 1
    val minY = pixels.map(_._2).min - 1
    val maxY = pixels.map(_._2).max + 1
    (for (x <- minX to maxX; y <- minY to maxY) yield {
      val bin = Seq(
        (x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
        (x - 1, y), (x, y), (x + 1, y),
        (x - 1, y + 1), (x, y + 1), (x + 1, y + 1))
        .map(p => if (p._1 <= minX || p._1 >= maxX || p._2 <= minY || p._2 >= maxY) isBorderOn else pixels.contains(p))
        .map(b => if (b) "1" else "0").mkString
      val index = Integer.parseInt(bin, 2)
      ((x, y), algo(index) == '#')
    }).filter(_._2).map(_._1).toSet
  }

  val iter1 = run(pixels, false)
  val part1 = run(iter1, true).size
  println(part1)

  val part2 = (1 to 50).foldLeft(pixels)((px, i) => {
    //println(i)
    run(px, i % 2 == 0)
  }).size
  println(part2)

}
