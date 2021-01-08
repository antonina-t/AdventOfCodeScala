package aoc2015

import scala.io.Source
import scala.util.Using

object Day14 extends App {
  val input = Using(Source.fromFile("input/2015/14.txt")) {
    _.getLines.toSeq
  }.get

  val time = 2503

  val pattern = "(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds.".r

  val reindeer = input.map {
    case pattern(name, speed, flyTime, restTime) => name -> (speed.toInt, flyTime.toInt, restTime.toInt)
  }.toMap

  val part1 = reindeer.map {
    case (_, (speed, flyTime, restTime)) =>
      val period = flyTime + restTime
      (time / period * flyTime + Math.min(time % period, flyTime)) * speed
  }.max
  println(part1)

  val part2 = (0 until time)
    .foldLeft(reindeer.keys.map((_, 0, 0)))((acc, timePassed) => {
      val accWithUpdatedDistance = acc.map {
        case (name, distance, score) =>
          val (speed, flyTime, restTime) = reindeer(name)
          val isFlying = timePassed % (flyTime + restTime) < flyTime
          (name, distance + (if (isFlying) speed else 0), score)
      }
      val leadDistance = accWithUpdatedDistance.map { case (_, distance, _) => distance }.max
      accWithUpdatedDistance.map {
        case entry@(name, distance, score) =>
          if (distance == leadDistance) (name, distance, score + 1) else entry
      }
    }).map { case (_, _, score) => score }.max
  println(part2)
}
