package aoc2024

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val input = Using(Source.fromFile("input/2024/11.txt")) {
    _.getLines().toSeq
  }.get.head.split(" ").map(_.toLong)

  def blink(stones: Seq[Long], blinks: Int): Int = {
    if (blinks == 0) stones.size
    else {
      val newStones = stones.flatMap(stone => {
        if (stone == 0)
          Seq(1L)
        else if (stone.toString.length % 2 == 0) {
          Seq(stone.toString.take(stone.toString.length / 2).toLong, stone.toString.drop(stone.toString.length / 2).toLong)
        } else {
          Seq(stone * 2024)
        }
      })
      blink(newStones, blinks - 1)
    }
  }

  val part1 = blink(input, 25)
  println(part1)

  val memo = scala.collection.mutable.Map[(Long, Int), Long]()

  def blink(stone: Long, blinks: Int): Long = {
    if (memo.contains((stone, blinks)))
      memo((stone, blinks))
    else {
      val result = if (blinks == 0)
        1
      else {
        if (stone == 0)
          blink(1, blinks - 1)
        else {
          val length = stone.toString.length
          if (length % 2 != 0) {
            blink(stone * 2024, blinks - 1)
          } else {
            blink(stone.toString.take(length / 2).toLong, blinks - 1) + blink(stone.toString.drop(length / 2).toLong, blinks - 1)
          }
        }
      }
      memo((stone, blinks)) = result
      result
    }
  }

  val part2 = input.map(stone => blink(stone, 75)).sum
  println(part2)

}
