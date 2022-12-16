import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format:
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
// Valve BB has flow rate=13; tunnels lead to valves CC, AA

sealed case class ValveDesc(name: String, flowRate: Int, tunnels: List[String])

object Parser extends RegexParsers with PackratParsers {
    def name: Parser[String] = """\w+""".r
    def flowRate: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def tunnels: Parser[List[String]] = rep1sep(name, ",")
    def valve: Parser[ValveDesc] = "Valve" ~> name ~ ("has flow rate=" ~> flowRate) ~ (";" ~> ("tunnels lead to valves" | "tunnel leads to valve") ~> tunnels) ^^ { case n ~ f ~ t => ValveDesc(n, f, t) }
    def problem: Parser[List[ValveDesc]] = rep1sep(valve, "")
}

val rawProblem = Parser.parseAll(Parser.problem, input)
val problem = rawProblem.get

val nameToId = problem.zipWithIndex.map(kvp => kvp._1.name -> kvp._2).toMap
val idToName = nameToId.map(_.swap)

sealed case class Tunnel(to: Int, tick: Int)

sealed case class Valve(id: Int, flowRate: Int, tunnels: List[Int])

val valves = problem.map(v => Valve(nameToId(v.name), v.flowRate, v.tunnels.map(nameToId))).toArray

final def pathBetweenValves(from: Int, to: Int, visited: Set[Int]): List[Int] = {
    if from == to then List(from)
    else {
        valves(from)
            .tunnels
            .filterNot(visited.contains)
            .map(newFrom => pathBetweenValves(newFrom, to, visited + from))
            .filter(_.nonEmpty)
            .minByOption(_.length)
            .map(from :: _)
            .getOrElse(Nil)
    }
}

val valvesCount = valves.length

def getPathId(from: Int, to: Int): Int = from * valvesCount + to

val timeBetween = Array.fill(valvesCount * valvesCount)(-1)

for {
    from <- 0 until valves.length
    to <- 0 until valves.length
    id = getPathId(from, to)
} {
    val path = pathBetweenValves(from, to, Set.empty)
    timeBetween(id) = path.length
}

val valvesToOpen = valves.filter(_.flowRate > 0).map(_.id).toList

def idToBitmap(id: Int): Long = 1L << id
val valvesToOpenAsBitMap = valvesToOpen.map(1L << _).sum

sealed case class Agent(agentId: Int, location: Int, timeRemaining: Int)

sealed case class State(agents: Array[Agent], valvesToOpen: Long, pressureExpected: Int) {
    def currentAgent = agents.maxBy(_.timeRemaining)
}

def pressureFrom(id: Int, timeRemaing: Int): Int = {
    val valve = valves(id)
    valve.flowRate * timeRemaing
}


def solve(state: State): State = {
    val agent = state.currentAgent
    if (agent.timeRemaining == 0 || state.valvesToOpen == 0) {
        return state
    } else {
        val agentLocation = agent.location
        
        var bestSubState: State = state
        var id = valvesCount

        while (id >= 0) {
            val bitMap = idToBitmap(id)
            if (bitMap & state.valvesToOpen) != 0 then {
                val timeToOpen = timeBetween(getPathId(agentLocation, id))
                val newTimeRemaining = agent.timeRemaining - timeToOpen
                if (newTimeRemaining >= 0) {
                    val newToOpen = state.valvesToOpen & ~bitMap
                    val newPressure = state.pressureExpected + pressureFrom(id, newTimeRemaining)
                    val newAgent = Agent(agent.agentId, id, newTimeRemaining)
                    val newAgents = state.agents.map(a => if a.agentId == agent.agentId then newAgent else a)
                    val newState = State(newAgents, newToOpen, newPressure)
                    val subState = solve(newState)
                    if subState.pressureExpected > bestSubState.pressureExpected then {
                        bestSubState = subState
                    }
                }
            }

            id = id - 1
        }

        bestSubState
    }
}

val part1InitialState = State(
    agents = Array(Agent(0, nameToId("AA"), 30)),
    pressureExpected = 0,
    valvesToOpen = valvesToOpenAsBitMap
)

val part1 = solve(part1InitialState)

val part2InitialState = State(
    agents = Array(Agent(0, nameToId("AA"), 26), Agent(1, nameToId("AA"), 26)),
    pressureExpected = 0,
    valvesToOpen = valvesToOpenAsBitMap
)

val part2 = solve(part2InitialState)