package aoc2022

import aoc2022.Day26.input

import scala.io.Source
import scala.util.Using

object Day26 extends App {

  val input = Using(Source.fromFile("input/2022/26.txt")) {
    _.getLines().toSeq
  }.get.map(_.split(" ").map(_.toInt)).toArray


  def findTriangleMaxPathTopDown(input: Array[Array[Int]]) = {
    def rowScores(prevRowScores: Array[Int], row: Array[Int]): Array[Int] = {
      row.zipWithIndex.map {
        case (nbr, i) =>
          nbr + Seq(i - 1, i).filter(prevRowScores.indices.contains).map(prevRowScores).max
      }
    }
    input.reduceLeft((prevRowScores, row) => rowScores(prevRowScores, row)).max
  }

  def findTriangleMaxPath(input: Array[Array[Int]]) = {
    def rowScores(prevRowScores: Array[Int], row: Array[Int]): Array[Int] = {
      row.zipWithIndex.map {
        case (nbr, i) =>
          nbr + Seq(i, i + 1).filter(prevRowScores.indices.contains).map(prevRowScores).max
      }
    }

    input.reduceRight((row, prevRowScores) => rowScores(prevRowScores, row)).head
  }

  println(findTriangleMaxPath(input))

}

