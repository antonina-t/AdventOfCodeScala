package aoc2015

object Day20 extends App {
  val input = 36000000

  val maxElf = input / 10
  val houses = new Array[Int](maxElf + 1)
  for (elf <- 1 to maxElf; house <- elf to maxElf by elf)
    yield houses(house) += elf * 10

  val part1 = houses.indices.dropWhile(houses(_) < input).head
  println(part1)

  val houses2 = new Array[Int](maxElf + 1)
  for (elf <- 1 to maxElf; house <- (elf to maxElf by elf).take(50))
    yield houses2(house) += elf * 11

  val part2 = houses2.indices.dropWhile(houses2(_) < input).head
  println(part2)
}
