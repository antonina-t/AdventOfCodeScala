package aoc2022

import scala.io.Source
import scala.util.Using

object Day7 extends App {

  val input = Using(Source.fromFile("input/2022/7.txt")) {
    _.getLines().toSeq
  }.get

  val dirs = input.foldLeft(("/", Map("/" -> Dir("/", 0, Seq.empty[String])))) { case ((path, dirs), line) =>
    if (line == "$ ls" || line == "$ cd /")
      (path, dirs)
    else if (!line.startsWith("$ cd ..") && line.startsWith("$ cd"))
      (path + "/" + line.substring(5), dirs)
    else if (line == "$ cd ..")
      (path.substring(0, path.lastIndexOf("/")), dirs)
    else if (line.startsWith("dir")) {
      val newDir = Dir(path + "/" + line.substring(4), 0, Seq.empty)
      (path, dirs ++ Map(path + "/" + line.substring(4) -> newDir, path -> dirs(path).copy(dirs = dirs(path).dirs :+ newDir.path)))
    } else {
      (path, dirs ++ Map(path -> dirs(path).copy(size = dirs(path).size + line.split(" ")(0).toLong)))
    }
  }._2

  val part1 = dirs.map {
    case (_, dir) =>
      val totalSize = dir.totalSize(dirs)
      if (totalSize <= 100000) totalSize else 0
  }.sum

  println(part1)

  val sizeToFree = 30000000 - (70000000 - dirs("/").totalSize(dirs))

  val part2 = dirs.values.map(_.totalSize(dirs)).toSeq.sorted.dropWhile(_ < sizeToFree).head
  println(part2)

}

case class Dir(path: String, size: Long, dirs: Seq[String]) {
  def totalSize(memo: Map[String, Dir]): Long = size + dirs.map(memo).map(dir => dir.totalSize(memo)).sum
}

