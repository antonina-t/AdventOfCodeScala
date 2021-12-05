package aoc2016

import scala.io.Source
import scala.util.Using

object Day7 extends App {
  val input = Using(Source.fromFile("input/2016/7.txt")) {
    _.getLines.toSeq
  }.get

  def partition(ip: String) = {
    val split = ip.split(raw"\[|\]").zipWithIndex
    val supernet = split.filter(_._2 % 2 == 0).map(_._1)
    val hypernet = split.filter(_._2 % 2 == 1).map(_._1)
    (supernet, hypernet)
  }

  def containsAbba(s: String) = s.sliding(4).exists(s => s(0) == s(3) && s(1) == s(2) && s(0) != s(1))

  def supportsTls(ip: String) = {
    val (supernet, hypernet) = partition(ip)
    supernet.exists(containsAbba) && !hypernet.exists(containsAbba)
  }

  def getAbas(s: String) = s.sliding(3).filter(s => s(0) == s(2) && s(0) != s(1))

  def supportsSsl(ip: String) = {
    val (supernet, hypernet) = partition(ip)
    val abas = supernet.flatMap(getAbas)
    val babs = hypernet.flatMap(getAbas).toSet
    abas.exists(aba => babs.contains(Seq(aba(1), aba(0), aba(1)).mkString))
  }

  val part1 = input.count(supportsTls)
  println(part1)

  val part2 = input.count(supportsSsl)
  println(part2)
}
