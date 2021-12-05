package aoc2021

import scala.io.Source
import scala.util.Using

object Day4 extends App {
  val input = Using(Source.fromFile("input/2021/4.txt")) {
    _.getLines.toSeq
  }.get

  val numbers = input.head.split(",").map(_.toInt)

  val boards = input.drop(2).sliding(5,  6)
    .map(_.map(_.split(" ").filter(_.nonEmpty).map(_.toInt))).toSeq

  def mark(number: Int)(board: Seq[Array[Int]]) =
    board.map(_.map(a => if (a == number) -1 else a))

  def isWinBoard(board: Seq[Array[Int]]) =
    board.exists(_.forall(_ == -1)) ||
      board.head.indices.exists(i => board.forall(line => line(i) == -1))

  def score(number: Int)(board: Seq[Array[Int]]) = board.map(_.filter(_ > 0).sum).sum * number

  val scores = numbers.foldLeft((boards, Seq[Int]()))((acc, number) => {
    val (oldBoards, scores) = acc
    if (scores.length == boards.length) acc
    else {
      val newBoards = oldBoards.map(mark(number))
      (newBoards.filterNot(isWinBoard), scores ++ newBoards.filter(isWinBoard).map(score(number)))
    }
  })._2

  val part1 = scores.head
  println(part1)

  val part2 = scores.last
  println(part2)

}
