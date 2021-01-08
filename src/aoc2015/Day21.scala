package aoc2015

import scala.annotation.tailrec

object Day21 extends App {

  case class Item(cost: Int, damage: Int, armor: Int)

  case class Player(hitPoints: Int, damage: Int, armor: Int)

  val weapons = Seq(
    Item(8, 4, 0),
    Item(10, 5, 0),
    Item(25, 6, 0),
    Item(40, 7, 0),
    Item(74, 8, 0)
  )
  val armor = Seq(
    Item(13, 0, 1),
    Item(31, 0, 2),
    Item(53, 0, 3),
    Item(75, 0, 4),
    Item(102, 0, 5)
  )
  val rings = Seq(
    Item(25, 1, 0),
    Item(50, 2, 0),
    Item(100, 3, 0),
    Item(20, 0, 1),
    Item(40, 0, 2),
    Item(80, 0, 3)
  )

  val hero = Player(100, 0, 0)
  val boss = Player(103, 9, 2)

  def getDmg(attacker: Player, defender: Player) =
    if (attacker.damage > defender.armor) attacker.damage - defender.armor else 1

  def getHit(attacker: Player, defender: Player) =
    Player(defender.hitPoints - getDmg(attacker, defender), defender.damage, defender.armor)

  @tailrec
  def isWin(hero: Player, boss: Player, isHerosTurn: Boolean): Boolean = {
    if (hero.hitPoints <= 0) false
    else if (boss.hitPoints <= 0) true
    else if (isHerosTurn) isWin(hero, getHit(hero, boss), isHerosTurn = false)
    else isWin(getHit(boss, hero), boss, isHerosTurn = true)
  }

  def equip(hero: Player, item: Option[Item]): Player =
    item.map(item => Player(hero.hitPoints, hero.damage + item.damage, hero.armor + item.armor)).getOrElse(hero)

  val battles = for {
    weapon <- weapons.map(Some(_))
    armor <- armor.map(Some(_)) :+ None
    ring1 <- rings.map(Some(_)) :+ None
    ring2 <- rings.map(Some(_)) :+ None if ring1.isEmpty && ring2.isEmpty || ring1 != ring2
  } yield {
    val cost = Seq(weapon, armor, ring1, ring2).flatten.map(_.cost).sum
    val equippedHero = Seq(weapon, armor, ring1, ring2).foldLeft(hero)((hero, item) => equip(hero, item))
    (cost, isWin(equippedHero, boss, isHerosTurn = true))
  }

  val part1 = battles.filter(_._2).minBy(_._1)._1
  println(part1)

  val part2 = battles.filterNot(_._2).maxBy(_._1)._1
  println(part2)
}
