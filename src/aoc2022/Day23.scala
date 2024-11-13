package aoc2022

import scala.io.Source
import scala.util.Using

object Day23 extends App {

  val input = Using(Source.fromFile("input/2022/23e.txt")) {
    _.getLines().toSeq
  }.get

  val dirs = Seq('N', 'S', 'W', 'E')

  case class P(x: Int, y: Int)

  val elves = input.zipWithIndex.foldLeft(Set.empty[P]) {
    case (acc, (line, y)) => acc ++ (for (x <- line.indices) yield if (line(x) == '#') Some(P(x, y)) else None).flatten
  }

  def executeRound(elves: Set[P], firstDir: Int): Set[P] = {
    //printElves(elves)
    val suggestions = elves.map(elf => {
      val allNeighbors =
        ((elf.x - 1) to (elf.x + 1)).map(x => P(x, elf.y - 1)) ++
        ((elf.x - 1) to (elf.x + 1)).map(x => P(x, elf.y + 1)) ++
        Seq(P(elf.x - 1, elf.y), P(elf.x + 1, elf.y))

      if (allNeighbors.forall(p => !elves.contains(p)))
        elf -> None
      else {
        val suggestion = (dirs.drop(firstDir) ++ dirs.take(firstDir)).map(dir => {
          val restOfElves = elves -- Set(elf)
          val checkIfOccupied = dir match {
            case 'N' => ((elf.x - 1) to (elf.x + 1)).map(x => P(x, elf.y - 1))
            case 'S' => ((elf.x - 1) to (elf.x + 1)).map(x => P(x, elf.y + 1))
            case 'W' => ((elf.y - 1) to (elf.y + 1)).map(y => P(elf.x - 1, y))
            case 'E' => ((elf.y - 1) to (elf.y + 1)).map(y => P(elf.x + 1, y))
          }
          if (checkIfOccupied.forall(p => !restOfElves.contains(p))) Some(checkIfOccupied(1)) else None
        }).find(_.isDefined).flatten
        elf -> suggestion
      }
    })

    suggestions.map {
      case entry@(elf, maybeNextMove) => maybeNextMove.map(suggestion =>
        if (!(suggestions -- Set(entry)).flatMap(_._2).contains(suggestion))
          suggestion
        else
          elf
      ).getOrElse(elf)
    }
  }

  def printElves(elves: Set[P]) = {
    val minX = elves.minBy(_.x).x
    val maxX = elves.maxBy(_.x).x
    val minY = elves.minBy(_.y).y
    val maxY = elves.maxBy(_.y).y
    for (y <- minY to maxY) {
      for (x <- minX to maxX)
        print(if (elves.contains(P(x, y))) "#" else ".")
      println()
    }
    println()

  }

  def move(rounds: Int, elves: Set[P], dir: Int): Set[P] =
    if (rounds == 0) elves
    else move(rounds - 1, executeRound(elves, dir), (dir + 1) % 4)

  val resultAfter10Rounds = move(10, elves, 0)

  val minX = resultAfter10Rounds.minBy(_.x).x
  val maxX = resultAfter10Rounds.maxBy(_.x).x
  val minY = resultAfter10Rounds.minBy(_.y).y
  val maxY = resultAfter10Rounds.maxBy(_.y).y

  val part1 = (for (y <- minY to maxY; x <- minX to maxX) yield (if (resultAfter10Rounds.contains(P(x, y))) 0 else 1)).sum
  println(part1)

  def moveUntilNotNeeded(round: Int, elves: Set[P], dir: Int): Int = {
    //println(round)
    val afterRound = executeRound(elves, dir)
    if (afterRound == elves)
      round
    else moveUntilNotNeeded(round + 1, afterRound, (dir + 1) % 4)
  }

  val part2 = moveUntilNotNeeded(1, elves, 0)
  println(part2)
}

