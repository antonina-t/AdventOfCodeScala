package aoc2015

import scala.io.Source
import scala.util.Using

object Day24 extends App {
  val input = Using(Source.fromFile("input/2015/24.txt")) {
    _.getLines.toSeq
  }.get

  val packages = input.map(_.toInt)

  val memo = scala.collection.mutable.Map[(Set[Int], Int, Int), Set[Set[Int]]]()

  def groupsOfNHavingSum(packages: Seq[Int], n: Int, sum: Int): Seq[Seq[Int]] = {
    if (n == 0 || sum <= 0) Seq.empty
    else if (n == 1) packages.filter(_ == sum).map(Seq(_))
    else packages.indices.flatMap { i =>
      groupsOfNHavingSum(packages.drop(i + 1), n - 1, sum - packages(i)).map(packages(i) +: _)
    }
  }

  def groupHavingSumExists(packages: Set[Int], sum: Int): Boolean = {
    if (packages.contains(sum)) true
    else {
      val candidates = packages.filter(_ < sum)
      candidates.exists(p => groupHavingSumExists(candidates - p, sum - p))
    }
  }

  def smallestGroupsHavingSum(packages: Seq[Int], sum: Int) =
    LazyList.from(1).map(groupsOfNHavingSum(packages, _, sum)).dropWhile(_.isEmpty).head

  val part1 = smallestGroupsHavingSum(packages, packages.sum / 3)
    .filter { group => groupHavingSumExists(packages.toSet -- group.toSet, packages.sum / 3) }
    .map(_.foldLeft(1L)(_ * _))
    .min
  println(part1)

  val part2 = smallestGroupsHavingSum(packages, packages.sum / 2)
    .filter { group =>
      groupHavingSumExists(group.toSet, packages.sum / 4) &&
        groupHavingSumExists(packages.toSet -- group.toSet, packages.sum / 4)
    }
    .map(smallestGroupsHavingSum(_, packages.sum / 4).map(_.foldLeft(1L)(_ * _)).min)
    .min
  println(part2)
}
