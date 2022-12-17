package aoc2022

import scala.io.Source
import scala.util.Using

object Day8 extends App {

  val input = Using(Source.fromFile("input/2022/8.txt")) {
    _.getLines().toSeq
  }.get

  val visible = Array.ofDim[Boolean](input.size, input.head.length);

  for (row <- 1 until visible.length - 1) {
    for (col <- 1 until visible.head.length - 1) {
      val tree = input(row)(col) - '0'
      val maxLeft = input(row).take(col).map(_ - '0').max
      val maxRight = input(row).drop(col + 1).map(_ - '0').max
      val maxTop = input.map(_(col)).take(row).map(_ - '0').max
      val maxBottom = input.map(_(col)).drop(row + 1).map(_ - '0').max
      val result = tree > Seq(maxLeft, maxRight, maxTop, maxBottom).min
      println(row, col, tree, Seq(maxLeft, maxRight, maxTop, maxBottom))
      visible(row)(col) = result
    }
  }

  val part1  = visible.map(_.count(_ == true)).sum + input.size * 2 + (input.head.length - 2) * 2

  println(part1)

  val scores = Array.ofDim[Int](input.size, input.head.length);
  def score(tree: Int, list: Seq[Int]) = list.takeWhile(_ < tree).size + (if (list.dropWhile(_ < tree).nonEmpty) 1 else 0)

  for (row <- visible.indices) {
    for (col <- visible.head.indices) {
      val tree = input(row)(col) - '0'
      val leftScore = score(tree, input(row).take(col).map(_ - '0').reverse)
      val maxRight = score(tree, input(row).drop(col + 1).map(_ - '0'))
      val maxTop = score(tree, input.map(_ (col)).take(row).map(_ - '0').reverse)
      val maxBottom = score(tree, input.map(_ (col)).drop(row + 1).map(_ - '0'))
      val result = leftScore * maxRight * maxTop * maxBottom
      println(row, col, tree, Seq(maxTop, leftScore, maxRight, maxBottom))
      scores(row)(col) = result
    }
  }

  val part2 = scores.map(_.max).max
  println(part2)

}

