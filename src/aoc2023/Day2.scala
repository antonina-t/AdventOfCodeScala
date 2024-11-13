package aoc2023

import scala.io.Source
import scala.util.Using

object Day2 extends App {

  val input = Using(Source.fromFile("input/2023/2.txt")) {
    _.getLines().toSeq
  }.get

  val part1 = input.map(s => {
    val split = s.split(": ")
    val game = split(0).split(" ")(1).toInt
    val sets = split(1).split("; ")
    val possible = sets.forall(set => set.split(", ").forall(cubes => {
      val cubesSplit = cubes.split(" ")
      val number = cubesSplit(0).toInt
      val color = cubesSplit(1)
      color match {
        case "red" => number <= 12
        case "green" => number <= 13
        case "blue" => number <= 14
      }
    }))
    if (possible) game else 0
  }).sum
  println(part1)

  val part2 = input.map(s => {
    val split = s.split(": ")
    val sets = split(1).split("[;,] ")
    val mins = sets.foldLeft((0, 0, 0))((acc, cubes) => {
      val (minRed, minGreen, minBlue) = acc
      val cubesSplit = cubes.split(" ")
      val number = cubesSplit(0).toInt
      val color = cubesSplit(1)
      val result = color match {
        case "red" => (Math.max(minRed, number), minGreen, minBlue)
        case "green" => (minRed, Math.max(minGreen, number), minBlue)
        case "blue" => (minRed, minGreen, Math.max(minBlue, number))
      }
      result
    })
    mins._1 * mins._2 * mins._3
  }).sum
  println(part2)

}

