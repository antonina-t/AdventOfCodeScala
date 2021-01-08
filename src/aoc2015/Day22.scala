package aoc2015

object Day22 extends App {

  case class Hero(hitPoints: Int, armor: Int, mana: Int, shield: Int, recharge: Int, lvlHard: Boolean = false)

  case class Boss(hitPoints: Int, armor: Int, damage: Int, poison: Int)

  case class Action(mana: Int, apply: (Hero, Boss) => (Hero, Boss))

  val spells = Seq(
    Action(53, (hero, boss) => (hero.copy(mana = hero.mana - 53), boss.copy(hitPoints = boss.hitPoints - 4))),
    Action(73, (hero, boss) => (hero.copy(mana = hero.mana - 73, hitPoints = hero.hitPoints + 2), boss.copy(hitPoints = boss.hitPoints - 2))),
    Action(113, (hero, boss) => (hero.copy(mana = hero.mana - 113, shield = 6, hitPoints = if (hero.shield > 0) 0 else hero.hitPoints), boss)),
    Action(173, (hero, boss) => (hero.copy(mana = hero.mana - 173, hitPoints = if (boss.poison > 0) 0 else hero.hitPoints), boss.copy(poison = 6))),
    Action(229, (hero, boss) => (hero.copy(mana = hero.mana - 229, recharge = 5, hitPoints = if (hero.recharge > 0) 0 else hero.hitPoints), boss))
  )

  val getHit = Action(0, (hero, boss) => (hero.copy(hitPoints = if (boss.damage > hero.armor) hero.hitPoints - (boss.damage - hero.armor) else hero.hitPoints - 1), boss))

  val hero = Hero(50, 0, 500, 0, 0)
  val boss = Boss(58, 0, 9, 0)

  def turn(hero: Hero, boss: Boss, action: Action): (Hero, Boss) = {
    val (hero1, boss1) = applyEffects(hero, boss)
    if (boss1.hitPoints <= 0 || hero1.hitPoints <= 0) (hero1, boss1)
    else if (hero1.mana < action.mana) (hero1.copy(hitPoints = 0), boss1)
    else action.apply(hero1, boss1)
  }

  def applyEffects(hero: Hero, boss: Boss) =
    (
      hero.copy(
        armor = if (hero.shield > 0) 7 else 0,
        shield = if (hero.shield > 0) hero.shield - 1 else 0,
        mana = if (hero.recharge > 0) hero.mana + 101 else hero.mana,
        recharge = if (hero.recharge > 0) hero.recharge - 1 else 0,
        hitPoints = if (hero.lvlHard) hero.hitPoints - 1 else hero.hitPoints),
      boss.copy(
        hitPoints = if (boss.poison > 0) boss.hitPoints - 3 else boss.hitPoints,
        poison = if (boss.poison > 0) boss.poison - 1 else 0)
    )

  val memo = scala.collection.mutable.Map[(Hero, Boss), Int]()

  def minManaToWin(hero: Hero, boss: Boss): Int = {
    memo.getOrElseUpdate((hero, boss),
      spells.map { spell =>
        turn(hero, boss, spell) match {
          case (hero, _) if hero.hitPoints <= 0 => Int.MaxValue
          case (_, boss) if boss.hitPoints <= 0 => spell.mana
          case (hero, boss) => turn(hero, boss, getHit) match {
            case (hero, _) if hero.hitPoints <= 0 => Int.MaxValue
            case (_, boss) if boss.hitPoints <= 0 => spell.mana
            case (hero, boss) =>
              val rest = minManaToWin(hero, boss)
              if (rest == Int.MaxValue) Int.MaxValue else spell.mana + rest
          }
        }
      }.min)
  }

  val part1 = minManaToWin(hero, boss)
  println(part1)

  val part2 = minManaToWin(hero.copy(lvlHard = true), boss)
  println(part2)
}
