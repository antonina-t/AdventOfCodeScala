package aoc2022

import aoc2022.Day19.Resources.{Clay, Geode, Obsidian, Ore, Resource}

import scala.io.Source
import scala.util.Using

object Day19 extends App {

  val input = Using(Source.fromFile("input/2022/19.txt")) {
    _.getLines().toSeq
  }.get


  val pattern = raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.".r


  object Resources extends Enumeration {
    type Resource = Value
    val Ore, Clay, Obsidian, Geode = Value
  }

  case class Robot(produces: Resource, cost: Map[Resource, Int] = Map.empty)

  case class State(produce: Map[Resource, Int], timeLeft: Int, resources: Map[Resource, Int])

  val blueprints = input.map {
    case pattern(blueprint, oreOre, clayOre, obsidianOre, obsidianClay, geodeOre, geodeObsidian) =>
      blueprint.toInt -> Seq(
        Robot(Ore, Map(Ore -> oreOre.toInt)),
        Robot(Clay, Map(Ore -> clayOre.toInt)),
        Robot(Obsidian, Map(Ore -> obsidianOre.toInt, Clay -> obsidianClay.toInt)),
        Robot(Geode, Map(Ore -> geodeOre.toInt, Obsidian -> geodeObsidian.toInt)),
      )
  }

  def maxGeodesRec(produce: Map[Resource, Int], timeLeft: Int, resources: Map[Resource, Int], blueprint: Seq[Robot], memo: scala.collection.mutable.Map[State, Int]): Int = {
    if (timeLeft == 0)
      resources.getOrElse(Geode, 0)
    else {
      val cappedProduce = produce.map {
        case (resource, amount) if resource == Geode => resource -> amount
        case (resource, amount) =>
          val maxNeeded = blueprint.map(_.cost.getOrElse(resource, 0)).max
          resource -> (if (amount > maxNeeded) maxNeeded else amount)
      }
      val cappedResources = resources.map {
        case (resource, amount) if resource == Geode => resource -> amount
        case (resource, amount) =>
          val maxNeeded = blueprint.map(_.cost.getOrElse(resource, 0)).max + (timeLeft - 1) * (blueprint.map(_.cost.getOrElse(resource, 0)).max - cappedProduce.getOrElse(resource, 0))
          resource -> (if (amount > maxNeeded) maxNeeded else amount)
      }

      memo.getOrElseUpdate(State(cappedProduce, timeLeft, cappedResources), {

      val canMakeRobots = blueprint.filter(_.cost.forall { case (resource, value) => cappedResources.getOrElse(resource, 0) >= value })
      val resourcesAfterProduce = produce.foldLeft(cappedResources) {
        case (resources, (resource, amount)) => resources ++ Map(resource -> (resources.getOrElse(resource, 0) + amount))
      }

      val candidates = canMakeRobots.map(newRobot => {
        val resourcesAfterBuild = newRobot.cost.foldLeft(resourcesAfterProduce) {
          case (resources, (costResource, costValue)) => resources ++ Map(costResource -> (resources(costResource) - costValue))
        }
        maxGeodesRec(cappedProduce ++ Map(newRobot.produces -> (cappedProduce.getOrElse(newRobot.produces, 0) + 1)), timeLeft - 1, resourcesAfterBuild, blueprint, memo)
      }) :+ maxGeodesRec(cappedProduce, timeLeft - 1, resourcesAfterProduce, blueprint, memo)

      candidates.max
    })
    }
  }

  def maxGeodes(blueprint: Seq[Robot], time: Int): Int = maxGeodesRec(Map(Ore -> 1), time, Map.empty, blueprint, scala.collection.mutable.Map.empty)

  val part1 = blueprints.map {
    case (number, robots) =>
      println("blueprint " + number)
      val result = maxGeodes(robots, 24)
      println("result: " + result)
      number * result
  }.sum

  println(part1)

  val part2 = blueprints.take(3).map {
    case (number, robots) =>
      println(number)
      val result = maxGeodes(robots, 32)
      println("result: " + result)
      result
  }.product

  println(part2)
}

