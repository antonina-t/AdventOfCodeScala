package aoc2024

import aoc2024.Day9.disk

import scala.io.Source
import scala.util.Using

object Day9 extends App {

  val input = Using(Source.fromFile("input/2024/9.txt")) {
    _.getLines().toSeq
  }.get.head

  val disk = input.foldLeft((Seq.empty[Int], true))((a, c) => {
    val (acc, isFile) = a
    val length = c - '0'
    if (isFile) {
      val index = if (acc.isEmpty) 0 else acc(acc.lastIndexWhere(_ != -1)) + 1
      val file = Array.fill(length)(index)
      (acc ++ file, false)
    } else {
      (acc ++ Array.fill(length)(-1), true)
    }
  })._1.toArray

  def reformat(disk: Array[Int]): Array[Int]  = {
    if (!disk.reverse.dropWhile(_ == -1).contains(-1))
      disk
    else {
      val index1 = disk.indexOf(-1)
      val index2 = disk.lastIndexWhere(_ != -1)
      disk(index1) = disk(index2)
      disk(index2) = -1
      reformat(disk)
    }
  }

  val reformattedDisk = reformat(disk.clone()).takeWhile(_ != -1)
  val part1 = reformattedDisk.indices.map(i => 0L + i * reformattedDisk(i)).sum
  println(part1)

  val disk2 = input.foldLeft((Seq.empty[(Int, Int)], true))((a, c) => {
    val (acc, isFile) = a
    val length = c - '0'
    if (isFile) {
      val index = if (acc.isEmpty) 0 else acc(acc.lastIndexWhere(_._1 != -1))._1 + 1
      val file = (index, length)
      (acc :+ file, false)
    } else {
      (acc :+ (-1, length), true)
    }
  })._1

  def reformat2(disk: Seq[(Int, Int)], fileId: Int): Seq[(Int, Int)] = {
    if (fileId == 0)
      disk
    else {
      val file = disk.find(_._1 == fileId).get
      val index = disk.indexWhere(e => e._1 == -1 && e._2 >= file._2)
      if (index == -1 || index > disk.indexOf(file))
        reformat2(disk, fileId - 1)
      else {
        val length = disk(index)._2
        val rest = disk.drop(index + 1).map(e => if (e._1 == fileId) (-1, e._2) else e)
        if (length == file._2)
          reformat2((disk.take(index) :+ file) ++ rest, fileId - 1)
        else
          reformat2((disk.take(index) :+ file :+ (-1, length - file._2)) ++ rest, fileId - 1)
      }
    }
  }

  val reformattedDisk2 = reformat2(disk2, disk2.findLast(_._1 != -1).get._1)
  val part2 = reformattedDisk2.foldLeft((0L, 0))((acc, e) => {
    val (sum, index) = acc
    (sum + (if (e._1 != -1) (index until index + e._2).map(i => (0L + i) * e._1).sum else 0), index + e._2)
  })._1

  println(part2)

}
