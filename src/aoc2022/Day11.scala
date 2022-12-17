package aoc2022

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val input = Using(Source.fromFile("input/2022/11.txt")) {
    _.getLines().toSeq
  }.get.map(_.trim)

  case class Monkey(index: Int, items: Seq[Long], op: Long => Long, test: Long => Boolean, ifTrue: Int, ifFalse: Int)

  val opPlus = "Operation: new = old \\+ (\\d+)".r
  val opTimes = "Operation: new = old \\* (\\d+)".r
  val opSquared = "Operation: new = old \\* old".r

  val monkeys = input.sliding(7, 7).foldLeft(Seq.empty[Monkey]) {
    case (monkeys, lines) => {
      val index = "Monkey (\\d):".r.findAllIn(lines.head).subgroups.head.toInt
      val items = "Starting items: (.*)".r.findAllIn(lines(1)).subgroups.head.split(", ").map(_.toLong)
      val op: Long => Long = lines(2) match {
        case opPlus(plus) => _ + plus.toLong
        case opSquared() => v => v * v
        case opTimes(times) => _ * times.toLong
      }
      val testValue = "Test: divisible by (\\d+)".r.findAllIn(lines(3)).subgroups.head.toInt
      val test: Long => Boolean = _ % testValue == 0
      val ifTrue = "If true: throw to monkey (\\d)".r.findAllIn(lines(4)).subgroups.head.toInt
      val ifFalse = "If false: throw to monkey (\\d)".r.findAllIn(lines(5)).subgroups.head.toInt
      monkeys :+ Monkey(index, items, op, test, ifTrue, ifFalse)
    }
  }

  def business(cycles: Int, reduceWorry: Long => Long) =
    (1 to cycles).foldLeft((monkeys, Map.empty[Int, Long])) {
      case ((cycleMs, inspections), _) => {
        cycleMs.indices.foldLeft((cycleMs, inspections)) {
          case ((ms, is), i) =>
            val monkey = ms(i)
            val theseInspections = monkey.items.size
            val newMonkeys = monkey.items.foldLeft(ms) {
              case (mms, item) =>
                val worry = reduceWorry(monkey.op(item))
                val newMonkey = if (monkey.test(worry)) monkey.ifTrue else monkey.ifFalse
                mms.map(m => if (m.index == newMonkey) m.copy(items = mms(newMonkey).items :+ worry) else m)
            }
            val newMonkeys1 = newMonkeys.map(m => if (m.index == monkey.index) m.copy(items = Seq.empty) else m)
            val inspections1 = is ++ Map(monkey.index -> (is.getOrElse(monkey.index, 0L) + theseInspections))
            (newMonkeys1, inspections1)
        }
      }
    }._2.values.toSeq.sorted.reverse.take(2).product

  val part1 = business(20, _ / 3)
  println(part1)

  val div = 11 * 19 * 7 * 17 * 3 * 5 * 13 * 2
  val part2 = business(10000, _ % div)
  println(part2)

}

