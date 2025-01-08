package aoc2024

import scala.io.Source
import scala.util.Using

object Day25 extends App {

  val input = Using(Source.fromFile("input/2024/25.txt")) {
    _.getLines().toSeq
  }.get

  val (locks, keys) = input.sliding(8,8).map(_.init).foldLeft((Seq.empty[Seq[Int]],Seq.empty[Seq[Int]]))((acc, in) => {
    val sig = in.head.indices.map(i => in.count(row => row(i) == '#'))
    if (in.head.distinct == "#")
      (acc._1 :+ sig, acc._2)
    else
      (acc._1, acc._2 :+ sig)
  })

  val part1 = locks.map(lock => keys.count(key => {
    lock.zip(key).forall(pair => pair._1 + pair._2 <= 7)
  })).sum

  println(part1)


}
