package aoc2015

object Day25 extends App {
  val trgRow = 2981
  val trgCol = 3075

  val part1 = LazyList.from(1)
    .scanLeft(20151125L, 1, 1) {
      case ((number, row, col), _) =>
        (
          (number * 252533) % 33554393,
          if (row > 1) row - 1 else col + 1,
          if (row > 1) col + 1 else 1
        )
    }
    .dropWhile { case (_, row, col) => row != trgRow || col != trgCol }
    .head
    ._1
  println(part1)
}
