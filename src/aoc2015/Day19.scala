package aoc2015

import scala.io.Source
import scala.util.Using

object Day19 extends App {
  val input = Using(Source.fromFile("input/2015/19.txt")) {
    _.getLines.toSeq
  }.get

  val pattern = "(.*) => (.*)".r
  val replacements = input.takeWhile(_.nonEmpty).map {
    case pattern(src, trg) => src -> atoms(trg)
  }

  def atoms(molecule: String) = "[A-Z][a-z]?".r.findAllIn(molecule).toSeq

  val srcMolecule = atoms(input.last)

  def getTrgMolecules(molecule: Seq[String]) = replacements.flatMap {
    case (src, trg) => molecule.indices.foldLeft(Seq[Seq[String]]())((acc, i) => {
      if (molecule.drop(i).head == src)
        acc :+ (molecule.take(i) ++ trg ++ molecule.drop(i + 1))
      else
        acc
    })
  }.distinct

  val part1 = getTrgMolecules(srcMolecule).length
  println(part1)

  // In each iteration number of atoms increases by 1. Rn*Y*Ar patterns can be treated as ( , )
  val part2 = srcMolecule.filterNot(Seq("Rn", "Y", "Ar").contains(_)).size - srcMolecule.count(_ == "Y") - 1
  println(part2)
}
