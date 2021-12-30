package aoc2021

import scala.io.Source
import scala.util.Using

object Day18 extends App {
  val input = Using(Source.fromFile("input/2021/18.txt")) {
    _.getLines.toSeq
  }.get

  def depths(s: String): Seq[(Int, Int)] = s.foldLeft((Seq.empty[(Int, Int)], 0))((acc, c) => {
    val (numbers, depth) = acc
    c match {
      case '[' => (numbers, depth + 1)
      case ']' => (numbers, depth - 1)
      case ',' => (numbers, depth)
      case number => (numbers :+ (("" + number).toInt, depth), depth)
    }
  })._1

  def explodeOne(depths: Seq[(Int, Int)]) = {
    val explodingIndices = depths.indices.filter(depths(_)._2 == 5).take(2)
    if (explodingIndices.isEmpty) depths
    else {
      val left = depths.indices.takeWhile(_ < explodingIndices.head).lastOption
      val right = depths.indices.dropWhile(_ <= explodingIndices.last).headOption
      val newNumbers = depths.indices.map(i =>
        if (left.exists(i < _)) depths(i)
        else if (left.contains(i)) (depths(i)._1 + depths(explodingIndices.head)._1, depths(i)._2)
        else if (right.contains(i)) (depths(i)._1 + depths(explodingIndices.last)._1, depths(i)._2)
        else if (right.exists(i > _)) depths(i)
        else (0, 6)
      )
      (newNumbers.take(explodingIndices.head) :+ (0, 4)) ++ newNumbers.drop(explodingIndices.last + 1)
    }
  }

  def splitOne(depths: Seq[(Int, Int)]) = {
    val splittingIndex = depths.indices.find(depths(_)._1 >= 10)
    splittingIndex.map(i =>
      (depths.take(i) :+ (depths(i)._1 / 2, depths(i)._2 + 1) :+ (depths(i)._1 / 2 + (depths(i)._1 % 2), depths(i)._2 + 1)) ++ depths.drop(i + 1)
    ).getOrElse(depths)
  }

  @scala.annotation.tailrec
  def reduce(depths: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    val exploded = explodeOne(depths)
    if (depths == exploded) {
      val split = splitOne(depths)
      if (depths == split)
        depths
      else
        reduce(split)
    } else
      reduce(exploded)
  }

  def add(depths1: Seq[(Int, Int)], depths2: Seq[(Int, Int)]): Seq[(Int, Int)] =
    reduce(depths1.map { case (value, depth) => (value, depth + 1) } ++ depths2.map { case (value, depth) => (value, depth + 1) })

  @scala.annotation.tailrec
  def magnitude(depths: Seq[(Int, Int)]): Int = {
    if (depths.size == 1) depths.head._1
    else {
      val maxDepth = depths.maxBy(_._2)._2
      val left = depths.takeWhile(_._2 < maxDepth)
      val reducedPair = depths.slice(left.size, left.size + 2) match {
        case Seq((a, b), (c, _)) => (a * 3 + c * 2, b - 1)
      }
      magnitude((left :+ reducedPair) ++ depths.drop(left.size + 2))
    }
  }

  val numbers = input.map(depths)
  val part1 = magnitude(numbers.map(reduce).reduce(add))
  println(part1)

  val part2 = (for (n1 <- numbers; n2 <- numbers if (n1 != n2)) yield magnitude(add(n1, n2))).max
  println(part2)

}
