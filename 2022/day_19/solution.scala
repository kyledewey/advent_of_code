case class Cost(
  ore: Int,
  clay: Int,
  obsidian: Int)

case class Blueprint(
  id: Int,
  oreRobotCost: Cost,
  clayRobotCost: Cost,
  obsidianRobotCost: Cost,
  geodeRobotCost: Cost) {

  def maybeBuildAndCollect(state: State): Iterator[State] = {
    // collect without building a robot
    Iterator(state.collectResources) ++
    // build a robot
    state.decrementCost(oreRobotCost).map(_.collectResources.incrementOreRobots) ++
    state.decrementCost(clayRobotCost).map(_.collectResources.incrementClayRobots) ++
    state.decrementCost(obsidianRobotCost).map(_.collectResources.incrementObsidianRobots) ++
    state.decrementCost(geodeRobotCost).map(_.collectResources.incrementGeodeRobots)
  }

  def allFinalStates(numMinutes: Int, state: State): Iterator[State] = {
    if (numMinutes <= 0) {
      Iterator(state)
    } else {
      for {
        afterStep <- maybeBuildAndCollect(state)
        finalState <- allFinalStates(numMinutes - 1, afterStep)
      } yield finalState
    }
  }

  def maxNumGeodesIn(numMinutes: Int, state: State): Int = {
    allFinalStates(numMinutes, state).map(_.numGeodes).max
  }

  def qualityLevel(numMinutes: Int): Int = id * maxNumGeodesIn(numMinutes, State.initialState)
}


class BlueprintParseException extends Exception

object Blueprint {
  import scala.util.matching.Regex
  val BlueprintPattern = """^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$""".r

  def parseLineAsBlueprint(line: String): Blueprint = {
    line match {
      case BlueprintPattern(id, oreRobot, clayRobot, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian) =>
        Blueprint(
          id.toInt,
          Cost(oreRobot.toInt, 0, 0),
          Cost(clayRobot.toInt, 0, 0),
          Cost(obsidianRobotOre.toInt, obsidianRobotClay.toInt, 0),
          Cost(geodeRobotOre.toInt, 0, geodeRobotObsidian.toInt))
      case _ => throw new BlueprintParseException()
    }
  }

  def parseBlueprints(filename: String): Seq[Blueprint] = {
    import scala.io.Source
    Source.fromFile(filename).getLines().map(parseLineAsBlueprint _).toSeq
  }
}

// goal: maximize the number of opened geodes
case class State(
  numOre: Int,
  numClay: Int,
  numObsidian: Int,
  numGeodes: Int,
  numOreRobots: Int,
  numClayRobots: Int,
  numObsidianRobots: Int,
  numGeodeRobots: Int) {
  def collectResources: State = {
    copy(
      numOre = numOre + numOreRobots,
      numClay = numClay + numClayRobots,
      numObsidian = numObsidian + numObsidianRobots,
      numGeodes = numGeodes + numGeodeRobots)
  }
  def incrementOreRobots: State = copy(numOreRobots = numOreRobots + 1)
  def incrementClayRobots: State = copy(numClayRobots = numClayRobots + 1)
  def incrementObsidianRobots: State = copy(numObsidianRobots = numObsidianRobots + 1)
  def incrementGeodeRobots: State = copy(numGeodeRobots = numGeodeRobots + 1)

  def decrementCost(cost: Cost): Option[State] = {
    if (numOre >= cost.ore && numClay >= cost.clay && numObsidian >= cost.obsidian) {
      Some(
        copy(
          numOre = numOre - cost.ore,
          numClay = numClay - cost.clay,
          numObsidian = numObsidian - cost.obsidian))
    } else {
      None
    }
  }
}

object State {
  val initialState = State(0, 0, 0, 0, 1, 0, 0, 0)
}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Needs a file containing blueprints and the number of minutes")
    } else {
      val numMinutes = args(1).toInt
      println(Blueprint.parseBlueprints(args(0)).map(_.qualityLevel(numMinutes)).sum)
    }
  }
}
