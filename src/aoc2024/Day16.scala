package aoc2024

import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val input = Using(Source.fromFile("input/2024/16.txt")) {
    _.getLines().toSeq
  }.get


  val startRow = input.indexWhere(_.contains("S"))
  val startCol = input(startRow).indexOf("S")
  val start = P(startRow, startCol)

  val memo = collection.mutable.Map(R(start, 0) -> (Set(Set(start)), 0))
  def walk(paths: Set[(Set[(R, Int)], R, Int)]): (Set[Set[P]], Int) = {
    //println(paths.map(_._2.p).mkString(","))
    if (paths.isEmpty)
      (Set.empty, Integer.MAX_VALUE)
    else if (paths.forall(_._2.c(input) == 'E')) {
      val min = paths.minBy(_._3)._3
      val pPaths = paths.filter(_._3 == min).map(_._1.map(_._1.p))
      (pPaths, min)
    } else {
      val newPaths = paths.flatMap(t => {
        val (path, r, score) = t
        if (r.c(input) == 'E')
          Set(t)
        else {
          val p = r.p
          val dir = r.dir
          val right = (R(P(p.row, p.col + 1), 0), score + 1 + (if (dir == 3) 1 else dir) * 1000)
          val down = (R(P(p.row + 1, p.col), 1), score + 1 + (if ((dir + 3) % 4 == 3) 1 else (dir + 3) % 4) * 1000)
          val left = (R(P(p.row, p.col - 1), 2), score + 1 + (if ((dir + 2) % 4 == 3) 1 else (dir + 2) % 4) * 1000)
          val up = (R(P(p.row - 1, p.col), 3), score + 1 + (if ((dir + 1) % 4 == 3) 1 else (dir + 1) % 4) * 1000)

          val next = Set(right, down, left, up).filterNot(_._1.c(input) == '#').filterNot(n => path.map(_._1.p).contains(n._1.p))
          if (next.isEmpty)
            Set.empty
          else
            next.flatMap(n =>
              if (memo.contains(n._1)) {
                if (memo(n._1)._2 < n._2) {
                  Set.empty
                } else if (memo(n._1)._2 == n._2) {
                  val newPaths = memo(n._1)._1 ++ Set(path.map(_._1.p) + n._1.p)
                  memo(n._1) = (newPaths, n._2)
                  Set((path + n, n._1, n._2))
                } else {
                  memo(n._1) = (Set(path.map(_._1.p) + n._1.p), n._2)
                  Set((path + n, n._1, n._2))
                }
              } else {
                memo(n._1) = (Set(path.map(_._1.p) + n._1.p), n._2)
                Set((path + n, n._1, n._2))
              }
            )
        }
      })

      val prunedPaths = newPaths.filter(t => {
        val (path, _, _) = t
        path.forall(t => {
          val (r, score) = t
          memo(r)._2 == score
        })
      })

      walk(prunedPaths)
    }
  }
  val result = walk(Set((Set((R(start, 0), 0)), R(start, 0), 0)))
  val part1 = result._2
  val part2 = result._1.flatten.size
  println(part1)
  println(part2)
}

case class R(p: P, dir: Int) {
  def c(input: Seq[String]) = input(p.row)(p.col)
}
