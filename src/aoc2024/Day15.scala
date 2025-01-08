package aoc2024

import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val input = Using(Source.fromFile("input/2024/15.txt")) {
    _.getLines().toSeq
  }.get

  val inputGrid = input.takeWhile(_.nonEmpty)
  val plan = input.drop(inputGrid.size + 1).mkString
  def getIndices(grid: Seq[String]) = (for (row <- grid.indices; col <- grid.head.indices) yield (row, col))

  def move(grid: Seq[String], posRows: Seq[Seq[P]], delta: Int): Seq[String] = {
    val pos = posRows.last
    if (pos.isEmpty) {
      val posToMove = posRows.flatten.toSet
      getIndices(grid).map(i => {
        val bracketMovesHere = posToMove.find(p => p.row == i._1 - delta && p.col == i._2)
        bracketMovesHere.map(p => grid(p.row)(p.col)).orElse {
          val bracketMovesFromHere = posToMove.find(p => p.row == i._1 && p.col == i._2)
          bracketMovesFromHere.map(_ => '.')
        }.getOrElse(grid(i._1)(i._2))
      }).sliding(grid.head.length, grid.head.length).map(_.mkString).toSeq
    } else {
      if (pos.exists(p => grid(p.row + delta)(p.col) == '#'))
        grid
      else {
        val newPos = pos.flatMap(p => {
          val c = grid(p.row + delta)(p.col)
          if (c == ']')
            Seq(P(p.row + delta, p.col - 1), P(p.row + delta, p.col))
          else if (c == '[')
            Seq(P(p.row + delta, p.col + 1), P(p.row + delta, p.col))
          else
            Seq.empty
        })
        move(grid, posRows ++ Seq(newPos), delta)
      }
    }
  }

  def walk(grid: Seq[String], plan: String): Int = {
    println(plan.headOption.getOrElse(""))
    grid.foreach(println)
    println()

    val indices = getIndices(grid)
    if (plan.isEmpty) {
      indices.filter(i => "0[".contains(grid(i._1)(i._2))).map(i => i._1 * 100 + i._2).sum
    } else {
      val robot = indices.find(i => grid(i._1)(i._2) == '@').get
      val nextMove = plan.head
      nextMove match {
        case '^' if grid.head.length == wideGrid.head.length && "[]".contains(grid(robot._1 - 1)(robot._2)) =>
          val newGrid = move(grid, Seq(Seq(P(robot._1, robot._2))), -1)
          walk(newGrid, plan.tail)
        case 'v' if grid.head.length == wideGrid.head.length && "[]".contains(grid(robot._1 + 1)(robot._2)) =>
          val newGrid = move(grid, Seq(Seq(P(robot._1, robot._2))), 1)
          walk(newGrid, plan.tail)
        case '^' =>
          val above = grid.take(robot._1 + 1).map(_(robot._2)).mkString
          val emptySpace = above.lastIndexOf(".")
          if (emptySpace != -1 && !above.drop(emptySpace).contains("#")) {
            val move = above.take(emptySpace) + above.drop(emptySpace).tail + "."
            val newGrid = indices.map(i => {
              if (i._1 <= robot._1 && i._2 == robot._2)
                  move(i._1)
              else
                  grid(i._1)(i._2)
            }).sliding(grid.head.length, grid.head.length).map(_.mkString).toSeq
            walk(newGrid, plan.tail)
          } else {
            walk(grid, plan.tail)
          }
        case 'v' =>
          val below = grid.drop(robot._1).map(_(robot._2)).mkString
          val emptySpace = below.indexOf(".")
          if (emptySpace != -1 && !below.take(emptySpace).contains("#")) {
            val move = "." + below.take(emptySpace) + below.drop(emptySpace).tail
            val newGrid = indices.map(i => {
              if (i._1 >= robot._1 && i._2 == robot._2)
                move(i._1 - robot._1)
              else
                grid(i._1)(i._2)
            }).sliding(grid.head.length, grid.head.length).map(_.mkString).toSeq
            walk(newGrid, plan.tail)
          } else {
            walk(grid, plan.tail)
          }
        case '<' =>
          val left = grid(robot._1).take(robot._2 + 1)
          val emptySpace = left.lastIndexOf(".")
          if (emptySpace != -1 && !left.drop(emptySpace).contains("#")) {
            val move = left.take(emptySpace) + left.drop(emptySpace).tail + "."
            val newGrid = indices.map(i => {
              if (i._1 == robot._1 && i._2 <= robot._2)
                move(i._2)
              else
                grid(i._1)(i._2)
            }).sliding(grid.head.length, grid.head.length).map(_.mkString).toSeq
            walk(newGrid, plan.tail)
          } else {
            walk(grid, plan.tail)
          }
        case '>' =>
          val right = grid(robot._1).drop(robot._2)
          val emptySpace = right.indexOf(".")
          if (emptySpace != -1 && !right.take(emptySpace).contains("#")) {
            val move = "." + right.take(emptySpace) + right.drop(emptySpace).tail
            val newGrid = indices.map(i => {
              if (i._1 == robot._1 && i._2 >= robot._2)
                move(i._2 - robot._2)
              else
                grid(i._1)(i._2)
            }).sliding(grid.head.length, grid.head.length).map(_.mkString).toSeq
            walk(newGrid, plan.tail)
          } else {
            walk(grid, plan.tail)
          }
      }
    }
  }


  val wideGrid = inputGrid.map(row => row.flatMap(_ match {
    case '#' => "##"
    case '.' => ".."
    case 'O' => "[]"
    case '@' => "@."
  }))

  val part1 = walk(inputGrid, plan)
  println(part1)

  val part2 = walk(wideGrid, plan)
  println(part2)


}
