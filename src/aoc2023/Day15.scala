package aoc2023

import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val input = Using(Source.fromFile("input/2023/15.txt")) {
    _.getLines().next().split(",")
  }.get

  val part1 = input.map(s => {
    s.foldLeft(0)((acc, c) => {
      ((acc + c) * 17) % 256
    })
  }).sum

  println(part1)

  val boxes = Array.fill(256)(Seq.empty[(String, Int)])

  input.foreach(s => {
    val label = s.takeWhile(_.isLetter)
    val hash = label.foldLeft(0)((acc, c) => {
      ((acc + c) * 17) % 256
    })
    val op = s.dropWhile(_.isLetter).head
    if (op == '-')
      boxes(hash) = boxes(hash).filterNot(_._1 == label)
    else {
      val focalLength = s.split("=")(1).toInt
      val lenses = boxes(hash).map(lens => {
        val (lensLabel, oldFocalLength) = lens
        if (lensLabel == label)
          (lensLabel, focalLength)
        else
          (lensLabel, oldFocalLength)
      })
      boxes(hash) = if (lenses.exists(_._1 == label)) lenses else lenses :+ (label, focalLength)
    }
  })

  val part2 = boxes.indices.map(i => {
    boxes(i).indices.map(j => {
      val (_, focalLength) = boxes(i)(j)
      (i + 1) * (j + 1) * focalLength
    }).sum
  }).sum

  println(part2)

}

