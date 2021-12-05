package aoc2016

object Day11 extends App {

  // Assign a unique letter to each element. Lowercase for microchips, uppercase for generators.
  // Sort floor contents alphabetically.
  // Format: "elevator:floor0;floor1;floor2;floor3"
  val initialState = "0:LSTt;ls;PRpr;"
  val targetState = "3:;;;LPRSTlprst"

  def isValid(state: String) = {
    state.drop(2).split(";").forall(floor => {
      val ms = floor.filter(_.isLower).toSet
      val gs = floor.filter(_.isUpper).map(_.toLower).toSet
      gs.isEmpty || (ms -- gs).isEmpty
    })
  }

  def moveObjs(state: String, choose: String => Seq[String], elevatorUp: Boolean): Set[String] = {
    val floors = state.drop(2).split(";", -1)
    val elevatorFrom = state.take(1).toInt
    val elevatorTo = elevatorFrom + (if (elevatorUp) 1 else -1)
    if (floors.indices.contains(elevatorTo))
      choose(floors(elevatorFrom))
        .map(objs => elevatorTo.toString + ":" + floors.indices.map(i =>
          if (i == elevatorFrom) floors(i).filterNot(objs.contains(_))
          else if (i == elevatorTo) (floors(i) + objs).sorted
          else floors(i)
        ).mkString(";"))
        .filter(isValid)
        .toSet
    else Set()
  }

  val chooseOne: String => Seq[String] = _.map(_.toString)

  val chooseTwo: String => Seq[String] = s => for (c1 <- s; c2 <- s if c1 != c2) yield s"$c1$c2"

  def getMoves(state: String): Set[String] = {
    val oneObjUp = moveObjs(state, chooseOne, elevatorUp = true)
    val oneObjDown = moveObjs(state, chooseOne, elevatorUp = false)
    val twoObjsUp = moveObjs(state, chooseTwo, elevatorUp = true)
    val twoObjsDown = moveObjs(state, chooseTwo, elevatorUp = false)
    oneObjUp ++ oneObjDown ++ twoObjsUp ++ twoObjsDown
  }

  def getMinSteps(initialState: String, targetState: String) = {
    val visited = scala.collection.mutable.Set(initialState)
    LazyList.from(1).scanLeft((Set(initialState), 0)) { case ((states, _), steps) =>
      val nextStates = states.flatMap(getMoves).diff(visited)
      visited.addAll(nextStates)
      println(steps + ": " + nextStates.size + " " + visited.size)
      (nextStates, steps)
    }
      .dropWhile { case (states, _) => !states.contains(targetState) }.head._2
  }

  val part1 = getMinSteps(initialState, targetState)
  println(part1)

  val initialState2 = "0:DELSTdet;ls;PRpr;"
  val targetState2 = "3:;;;DELPRSTdelprst"

  val part2 = getMinSteps(initialState2, targetState2)
  println(part2)
}
