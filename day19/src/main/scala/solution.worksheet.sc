import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format :
// Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 18 clay. Each geode robot costs 4 ore and 9 obsidian.
// Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 2 ore. Each obsidian robot costs 2 ore and 17 clay. Each geode robot costs 2 ore and 10 obsidian.
// ...

sealed case class Blueprint(
    id: Int,
    oreRobotOreCost: Int,
    clayRobotOreCost: Int,
    obsidianRobotOreCost: Int,
    obsidianRobotClayCost: Int,
    geodeRobotOreCost: Int,
    geodeRobotObsidianCost: Int
)

type Problem = List[Blueprint]

object Parser extends RegexParsers with PackratParsers {
    val number: Parser[Int] = """\d+""".r ^^ (_.toInt)
    val oreRobot: Parser[Int] = "Each ore robot costs" ~> number <~ "ore."
    val clayRobot: Parser[Int] = "Each clay robot costs" ~> number <~ "ore."
    val obsidianRobot: Parser[(Int, Int)] = ("Each obsidian robot costs" ~> number <~ "ore and") ~ (number <~ "clay.") ^^ { case oreCost ~ clayCost => (oreCost, clayCost) }
    val geodeRobot: Parser[(Int, Int)] = ("Each geode robot costs" ~> number <~ "ore and") ~ (number <~ "obsidian.") ^^ { case oreCost ~ obsidianCost => (oreCost, obsidianCost) }
    val blueprint: Parser[Blueprint] = ("Blueprint" ~> number <~ ":") ~ oreRobot ~ clayRobot ~ obsidianRobot ~ geodeRobot ^^
                                            { case id ~ oreRobotCost ~ clayRobotCost ~ (obsidianRobotOreCost, obsidianRobotClayCost) ~ (geodeRobotOreCost, geodeRobotObsidianCost) =>
                                                 Blueprint(id, oreRobotCost, clayRobotCost, obsidianRobotOreCost, obsidianRobotClayCost, geodeRobotOreCost, geodeRobotObsidianCost)
                                            }
    val problem: Parser[Problem] = blueprint.*
}

val problemRaw = Parser.parseAll(Parser.problem, input)
val problem = problemRaw.get

sealed trait RobotType
case object OreRobot extends RobotType
case object ClayRobot extends RobotType
case object ObsidianRobot extends RobotType
case object GeodeRobot extends RobotType

val buildList = List(OreRobot, ClayRobot, ObsidianRobot, GeodeRobot).toArray

sealed case class State(
    ore: Int,
    clay: Int,
    obsidian: Int,
    geode: Int,
    oreRobotCount: Int,
    clayRobotCount: Int,
    obsidianRobotCount: Int,
    geodeRobotCount: Int,
    blueprint: Blueprint,
    nextBuild: RobotType,
    remainingSteps: Int,
   // buildOrder: List[RobotType]
)

final def initialState(robot: RobotType, blueprint: Blueprint, remainingSteps: Int) = State(
    ore = 0,
    clay = 0,
    obsidian = 0,
    geode = 0,
    oreRobotCount = 1,
    clayRobotCount = 0,
    obsidianRobotCount = 0,
    geodeRobotCount = 0,
    blueprint = blueprint,
    nextBuild = robot,
    remainingSteps = remainingSteps,
    //buildOrder = Nil
)

final def isDeadEnd(state: State): Boolean = state.nextBuild match {
    case OreRobot => state.remainingSteps < 2
    case ClayRobot => state.remainingSteps < 3
    case ObsidianRobot => state.clayRobotCount == 0 || state.remainingSteps < 2
    case GeodeRobot => state.obsidianRobotCount == 0
}

final def canBuild(state: State): Boolean = {    
    val blueprint = state.blueprint
        
    state.nextBuild match {
        case OreRobot => state.ore >= state.blueprint.oreRobotOreCost && state.remainingSteps > 2
        case ClayRobot => state.ore >= state.blueprint.clayRobotOreCost && state.remainingSteps > 3
        case ObsidianRobot => state.ore >= state.blueprint.obsidianRobotOreCost && state.clay >= state.blueprint.obsidianRobotClayCost && state.remainingSteps > 2
        case GeodeRobot => state.ore >= state.blueprint.geodeRobotOreCost && state.obsidian >= state.blueprint.geodeRobotObsidianCost && state.remainingSteps > 1
    }
}

final def build(state: State): List[State] = {
    val blueprint = state.blueprint

    state.nextBuild match {
        case OreRobot =>
            //val newBuildOrder = state.nextBuild :: state.buildOrder
            val newOre = state.ore - blueprint.oreRobotOreCost
            val newOreRobotCount = state.oreRobotCount + 1
            buildList
                .view
                .map(robot => 
                    state.copy(
                        ore = newOre,
                        oreRobotCount = newOreRobotCount,
                        nextBuild = robot,
                        //buildOrder = newBuildOrder
                    )
                )
                .filterNot(isDeadEnd)
                .toList
        case ClayRobot =>
            //val newBuildOrder = state.nextBuild :: state.buildOrder
            val newOre = state.ore - blueprint.clayRobotOreCost
            val newClayRobotCount = state.clayRobotCount + 1
            buildList
                .view
                .map(robot => 
                    state.copy(
                        ore = newOre,
                        clayRobotCount = newClayRobotCount,
                        nextBuild = robot,
                        //buildOrder = newBuildOrder
                    )
                )
                .filterNot(isDeadEnd)
                .toList
        case ObsidianRobot =>
            //val newBuildOrder = state.nextBuild :: state.buildOrder
            val newOre = state.ore - blueprint.obsidianRobotOreCost
            val newClay = state.clay - blueprint.obsidianRobotClayCost
            val newObsidianRobotCount = state.obsidianRobotCount + 1
            buildList
                .view
                .map(robot => 
                    state.copy(
                        ore = newOre,
                        clay = newClay,
                        obsidianRobotCount = newObsidianRobotCount,
                        nextBuild = robot,
                        //buildOrder = newBuildOrder
                    )
                )
                .filterNot(isDeadEnd)
                .toList
        case GeodeRobot =>
            //val newBuildOrder = state.nextBuild :: state.buildOrder
            val newOre = state.ore - blueprint.geodeRobotOreCost
            val newObsidian = state.obsidian - blueprint.geodeRobotObsidianCost
            val newGeodeRobotCount = state.geodeRobotCount + 1
            buildList
                .view
                .map(robot => 
                    state.copy(
                        ore = newOre,
                        obsidian = newObsidian,
                        geodeRobotCount = newGeodeRobotCount,
                        nextBuild = robot,
                        //buildOrder = newBuildOrder
                    )
                )
                .filterNot(isDeadEnd)
                .toList
    }
}

final def generateRessources(state: State): State = {
    state.copy(
        ore = state.ore + state.oreRobotCount,
        clay = state.clay + state.clayRobotCount,
        obsidian = state.obsidian + state.obsidianRobotCount,
        geode = state.geode + state.geodeRobotCount,
        remainingSteps = state.remainingSteps - 1
    )
}

final def step(state: State): List[State] = {
    if (canBuild(state)) {
        build(generateRessources(state))
    } else {
        List(generateRessources(state))
    }
}

final def bestState(a: State, b: State): State = {
    if (a.geode > b.geode) {
        a
    } else {
        b
    }
}

@tailrec
final def solve(stateToSolve: List[State], accBestState: State, bestPerStep: Array[Int]): State = {
    stateToSolve match {
        case Nil => accBestState
        case state :: tail =>
            if (state.geode < bestPerStep(state.remainingSteps)) {
                solve(tail, accBestState, bestPerStep)
            } else {
                if (state.geode > bestPerStep(state.remainingSteps)) {
                    bestPerStep(state.remainingSteps) = state.geode
                }

                if (state.remainingSteps == 0) {
                    solve(tail, bestState(accBestState, state), bestPerStep)
                } else {
                    solve(step(state) ++ tail, accBestState, bestPerStep)
                }
                
            }
    }
}

def solveFor(blueprints: List[Blueprint], remainingSteps: Int): List[State] = {
    for {
        blueprint <- blueprints
    } yield {
        var best = initialState(OreRobot, blueprint, remainingSteps = 0)
        val bestPerStep = Array.fill(remainingSteps+1)(0)

        for {
            robot <- buildList 
            state = initialState(robot, blueprint, remainingSteps)
            if !isDeadEnd(state)
        } {
            best = solve(List(state), best, bestPerStep)
        }

        best
    }
}

//val part1Best = solveFor(problem, remainingSteps = 24)
//val part1 = part1Best.map(b => b.blueprint.id * b.geode).sum

// part 2
val part2Best = solveFor(problem.take(3), remainingSteps = 32)
val part2 = part2Best.map(b => b.geode).product