package aoc2021


import scala.io.Source
import scala.util.Using

object Day11 extends App {
  val input = Using(Source.fromFile("input/2021/11.txt")) {
    _.getLines.toSeq
  }.get.map(_.map(_ - '0').toArray).toArray

  val indices = (for (y <- input.indices; x <- input(0).indices) yield (y, x)).toSet

  def flash(grid: Array[Array[Int]], flashed: Set[(Int,Int)], flashes: Int, i:Int): (Array[Array[Int]], Int) = {
    if (flashed.size == indices.size) println("!!! " + i)
    if (!indices.diff(flashed).exists{case (y,x) => grid(y)(x) > 9}) (grid.map(_.map(e => if (e > 9) 0 else e)), flashes) else {
      val newGrid = grid.map(_.map(identity))
      val newFlashes = indices.diff(flashed).filter{case (y,x) => newGrid(y)(x) > 9}
      newFlashes.foreach { case (y,x) =>
        if (grid(y)(x) >= 9 && !flashed.contains((y,x))) {
          Set((y - 1, x - 1), (y - 1, x), (y - 1, x + 1),
            (y, x - 1), (y, x + 1),
            (y + 1, x - 1), (y + 1, x), (y + 1, x + 1))
            .intersect(indices).foreach { case (y, x) => newGrid(y)(x) = newGrid(y)(x) + 1 }
        }
      }
      flash(newGrid, flashed ++ newFlashes, flashes + newFlashes.size, i)
    }
  }


  val part1 = (1 to 1000).foldLeft((input, 0))((acc, i) => acc match {
    case (grid, sum) => {
      val newGrid = grid.map(_.map(_ + 1))
      flash(newGrid, Set.empty, sum, i)
    }
  })._2
  println(part1)

  val part2 = ""
  println(part2)

}
