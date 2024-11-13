package aoc2023

import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val inputS = Using(Source.fromFile("input/2023/10.txt")) {
    _.getLines().toSeq
  }.get

  val startY = inputS.indexWhere(_.contains('S'))
  val startX = inputS(startY).indexOf('S')

  val input = inputS.map(_.replace('S', '|'))

  def getNext(curr: (Int, Int), neq: Option[(Int, Int)]): (Int, Int) = {
    val (y, x) = curr
    (input(y)(x) match {
      case '|'  => Seq((y - 1, x), (y + 1, x))
      case '-' => Seq((y, x - 1), (y, x + 1))
      case 'F' => Seq((y + 1, x), (y, x + 1))
      case '7' => Seq((y + 1, x), (y, x - 1))
      case 'J' => Seq((y - 1, x), (y, x - 1))
      case 'L' => Seq((y - 1, x), (y, x + 1))
    }).filterNot(neq.contains).filter(point => input.indices.contains(point._1) && input.head.indices.contains(point._2)).head
  }

  def go(loop: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    if (loop.length > 1 && loop.head == loop.last) loop
    else {
      go(loop :+ getNext(loop.last, loop.init.lastOption))
    }
  }

  val loop = go(Seq((startY, startX)))
  val part1 = (loop.length - 1) / 2

  println(part1)

  val miniYs = input.indices.flatMap(i => Seq(2 * i, 2 * i + 1))
  val miniXs = input.head.indices.flatMap(i => Seq(2 * i, 2 * i + 1))

  val pipePartitions = Map(
    '|' -> Set(Set(0,2), Set(1, 3)),
    '-' -> Set(Set(0,1), Set(2, 3)),
    'F' -> Set(Set(0,1,2), Set(3)),
    '7' -> Set(Set(0,1,3), Set(2)),
    'L' -> Set(Set(0,2,3), Set(1)),
    'J' -> Set(Set(1,2,3), Set(0)),
  )

  def getNeighbours(miniY: Int, miniX: Int) = {
    val (y, x) = (miniY / 2, miniX / 2)

    val pos = miniX % 2 + 2 * (miniY % 2)

    val neighborsOutside = (pos match {
      case 0 => Set((miniY - 1, miniX), (miniY, miniX - 1))
      case 1 => Set((miniY - 1, miniX), (miniY, miniX + 1))
      case 2 => Set((miniY + 1, miniX), (miniY, miniX - 1))
      case 3 => Set((miniY + 1, miniX), (miniY, miniX + 1))
    }).filter{ case (y, x) => miniYs.contains(y) && miniXs.contains(x) }

    val allNeighborsInside = pos match {
      case 0 => Set(1, 2)
      case 1 => Set(0, 3)
      case 2 => Set(1, 3)
      case 3 => Set(1, 2)
    }

    val excludeNeighbors = if (!loop.contains((y,x))) Set.empty[Int] else
      pipePartitions(input(y)(x)).filterNot(_.contains(pos)).head

    val neighborsInside = (allNeighborsInside -- excludeNeighbors).map { pos =>
      (y * 2 + pos / 2, x * 2 + pos % 2)
    }

    neighborsOutside ++ neighborsInside
  }

  def bfs(q: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (q.isEmpty) visited
    else {
      val curr = q.head
      val neighbors = getNeighbours(curr._1, curr._2) -- visited
      bfs(q.tail ++ neighbors, visited ++ Set(curr))
    }
  }

  val outsideMiniTiles = bfs(Set((0,0)), Set.empty)

  val part2 = (for (y <- input.indices; x <- input.head.indices) yield (y , x))
    .filterNot(loop.contains)
    .filterNot{ case (y, x) => outsideMiniTiles.contains((y * 2, x * 2)) }
    .size

  println(part2)
}

